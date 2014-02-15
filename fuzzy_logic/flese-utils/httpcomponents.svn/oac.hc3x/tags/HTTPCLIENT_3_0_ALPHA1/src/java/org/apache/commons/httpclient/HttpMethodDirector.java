/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpMethodDirector.java,v 1.25 2004/05/13 04:03:25 mbecke Exp $
 * $Revision: 1.25 $
 * $Date: 2004-05-13 06:03:25 +0200 (Thu, 13 May 2004) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2004 The Apache Software Foundation
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.commons.httpclient;

import java.io.IOException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.httpclient.auth.AuthChallengeException;
import org.apache.commons.httpclient.auth.AuthChallengeParser;
import org.apache.commons.httpclient.auth.AuthChallengeProcessor;
import org.apache.commons.httpclient.auth.AuthScheme;
import org.apache.commons.httpclient.auth.AuthState;
import org.apache.commons.httpclient.auth.AuthenticationException;
import org.apache.commons.httpclient.auth.CredentialsProvider;
import org.apache.commons.httpclient.auth.CredentialsNotAvailableException;
import org.apache.commons.httpclient.auth.HttpAuthRealm;
import org.apache.commons.httpclient.auth.MalformedChallengeException;
import org.apache.commons.httpclient.params.HttpClientParams;
import org.apache.commons.httpclient.params.HttpConnectionParams;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.commons.httpclient.params.HttpParams;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Handles the process of executing a method including authentication, redirection and retries.
 * 
 * @since 3.0
 */
class HttpMethodDirector {

    /** The www authenticate challange header. */
    public static final String WWW_AUTH_CHALLENGE = "WWW-Authenticate";

    /** The www authenticate response header. */
    public static final String WWW_AUTH_RESP = "Authorization";

    /** The proxy authenticate challange header. */
    public static final String PROXY_AUTH_CHALLENGE = "Proxy-Authenticate";

    /** The proxy authenticate response header. */
    public static final String PROXY_AUTH_RESP = "Proxy-Authorization";

    private static final Log LOG = LogFactory.getLog(HttpMethodDirector.class);

    private ConnectMethod connectMethod;
    
    private HttpState state;
	
    private HostConfiguration hostConfiguration;
    
    private HttpConnectionManager connectionManager;
    
    private HttpClientParams params;
    
    private HttpConnection conn;
    
    /** A flag to indicate if the connection should be released after the method is executed. */
    private boolean releaseConnection = false;

    private static final int AUTH_UNINITIATED = 0;
    private static final int AUTH_PREEMPTIVE = 1;
    private static final int AUTH_WWW_REQUIRED = 2;
    private static final int AUTH_PROXY_REQUIRED = 3;
    private static final int AUTH_NOT_REQUIRED = Integer.MAX_VALUE;

    /** Actual state of authentication process */
    private int authProcess = AUTH_UNINITIATED;
    
    /** Authentication processor */
    private AuthChallengeProcessor authProcessor = null;

    private Set redirectLocations = null; 
    
    public HttpMethodDirector(
        final HttpConnectionManager connectionManager,
        final HostConfiguration hostConfiguration,
        final HttpClientParams params,
        final HttpState state
    ) {
        super();
        this.connectionManager = connectionManager;
        this.hostConfiguration = hostConfiguration;
        this.params = params;
        this.state = state;
        this.authProcessor = new AuthChallengeProcessor(this.params);
    }
    
	
    /**
     * Executes the method associated with this method director.
     * 
     * @throws IOException
     * @throws HttpException
     */
    public void executeMethod(final HttpMethod method) throws IOException, HttpException {
        if (method == null) {
            throw new IllegalArgumentException("Method may not be null");
        }
        // Link all parameter collections to form the hierarchy:
        // Global -> HttpClient -> HostConfiguration -> HttpMethod
        this.hostConfiguration.getParams().setDefaults(this.params);
        method.getParams().setDefaults(this.hostConfiguration.getParams());
        try {
            int maxRedirects = this.params.getIntParameter(HttpClientParams.MAX_REDIRECTS, 100);

            for (int redirectCount = 0;;) {

                // make sure the connection we have is appropriate
                if (this.conn != null && !hostConfiguration.hostEquals(this.conn)) {
                    this.conn.setLocked(false);
                    this.conn.releaseConnection();
                    this.conn = null;
                }
        
                // get a connection, if we need one
                if (this.conn == null) {
                    this.conn = connectionManager.getConnectionWithTimeout(
                        hostConfiguration,
                        this.params.getConnectionManagerTimeout() 
                    );
                    this.conn.setLocked(true);
                    if (this.params.isAuthenticationPreemptive()
                     || this.state.isAuthenticationPreemptive()) 
                    {
                        LOG.debug("Preemptively sending default basic credentials");
                        this.authProcess = AUTH_PREEMPTIVE;
                        method.getHostAuthState().setPreemptive();
                        if (this.conn.isProxied()) {
                            method.getProxyAuthState().setPreemptive();
                        }
                    }
                }
                authenticate(method);
                executeWithRetry(method);
                if (this.connectMethod != null) {
                    fakeResponse(method);
                    break;
                }
                
                boolean retry = false;
                if (isRedirectNeeded(method)) {
                    if (processRedirectResponse(method)) {
                        retry = true;
                        ++redirectCount;
                        if (redirectCount >= maxRedirects) {
                            LOG.error("Narrowly avoided an infinite loop in execute");
                            throw new RedirectException("Maximum redirects ("
                                + maxRedirects + ") exceeded");
                        }
                        if (LOG.isDebugEnabled()) {
                            LOG.debug("Execute redirect " + redirectCount + " of " + maxRedirects);
                        }
                    }
                }
                if (isAuthenticationNeeded(method)) {
                    if (processAuthenticationResponse(method)) {
                        retry = true;
                    }
                } else {
                    this.authProcess = AUTH_NOT_REQUIRED;
                }
                if (!retry) {
                    break;
                }
                // retry - close previous stream.  Caution - this causes
                // responseBodyConsumed to be called, which may also close the
                // connection.
                if (method.getResponseBodyAsStream() != null) {
                    method.getResponseBodyAsStream().close();
                }

            } //end of retry loop
        } finally {
            if (this.conn != null) {
                this.conn.setLocked(false);
            }
            // If the response has been fully processed, return the connection
            // to the pool.  Use this flag, rather than other tests (like
            // responseStream == null), as subclasses, might reset the stream,
            // for example, reading the entire response into a file and then
            // setting the file as the stream.
            if (
                (releaseConnection || method.getResponseBodyAsStream() == null) 
                && this.conn != null
            ) {
                this.conn.releaseConnection();
            }
        }

    }

    
    private void authenticate(final HttpMethod method) {
        try {
            authenticateProxy(method);
            authenticateHost(method);
        } catch (AuthenticationException e) {
            LOG.error(e.getMessage(), e);
        }
    }


    private boolean cleanAuthHeaders(final HttpMethod method, final String name) {
        Header[] authheaders = method.getRequestHeaders(name);
        boolean clean = true;
        for (int i = 0; i < authheaders.length; i++) {
            Header authheader = authheaders[i];
            if (authheader.isAutogenerated()) {
                method.removeRequestHeader(authheader);
            } else {
                clean = false;
            }
        }
        return clean;
    }
    

    private void authenticateHost(final HttpMethod method) throws AuthenticationException {
        // Clean up existing authentication headers
        if (!cleanAuthHeaders(method, WWW_AUTH_RESP)) {
            // User defined authentication header(s) present
            return;
        }
        AuthScheme authscheme = method.getHostAuthState().getAuthScheme();
        if (authscheme == null) {
            return;
        }
        if ((this.authProcess == AUTH_WWW_REQUIRED) || (!authscheme.isConnectionBased())) {
            String host = conn.getVirtualHost();
            if (host == null) {
                host = conn.getHost();
            }
            int port = conn.getPort();
            HttpAuthRealm realm = new HttpAuthRealm(
                host, port, 
                authscheme.getRealm(), 
                authscheme.getSchemeName());  
            if (LOG.isDebugEnabled()) {
                LOG.debug("Authenticating with " + realm);
            }
            Credentials credentials = this.state.getCredentials(realm);
            if (credentials != null) {
                String authstring = authscheme.authenticate(credentials, method);
                if (authstring != null) {
                    method.addRequestHeader(new Header(WWW_AUTH_RESP, authstring, true));
                }
            } else {
                LOG.warn("Required credentials not available");
            }
        }
    }


    private void authenticateProxy(final HttpMethod method) throws AuthenticationException {
        // Clean up existing authentication headers
        if (!cleanAuthHeaders(method, PROXY_AUTH_RESP)) {
            // User defined authentication header(s) present
            return;
        }
        AuthScheme authscheme = method.getProxyAuthState().getAuthScheme();
        if (authscheme == null) {
            return;
        }
        if ((this.authProcess == AUTH_PROXY_REQUIRED) || (!authscheme.isConnectionBased())) {
            HttpAuthRealm realm = new HttpAuthRealm(
                conn.getProxyHost(), conn.getProxyPort(), 
                authscheme.getRealm(), 
                authscheme.getSchemeName());  
            if (LOG.isDebugEnabled()) {
                LOG.debug("Authenticating with " + realm);
            }
            Credentials credentials = this.state.getProxyCredentials(realm);
            if (credentials != null) {
                String authstring = authscheme.authenticate(credentials, method);
                if (authstring != null) {
                    method.addRequestHeader(new Header(PROXY_AUTH_RESP, authstring, true));
                }
            } else {
                LOG.warn("Required credentials not available");
            }
        }
    }
    
    
    /**
     * Executes a method with the current hostConfiguration.
     *
     * @throws IOException if an I/O (transport) error occurs. Some transport exceptions 
     * can be recovered from.
     * @throws HttpException  if a protocol exception occurs. Usually protocol exceptions 
     * cannot be recovered from.
     */
    private void executeWithRetry(final HttpMethod method) 
        throws IOException, HttpException {
        
        /** How many times did this transparently handle a recoverable exception? */
        int recoverableExceptionCount = 0;
        int execCount = 0;
        // TODO: how do we get requestSent?
        boolean requestSent = false;
        
        // loop until the method is successfully processed, the retryHandler 
        // returns false or a non-recoverable exception is thrown
        try {
            while (true) {
                execCount++;
                requestSent = false;

                if (LOG.isTraceEnabled()) {
                    LOG.trace("Attempt number " + execCount + " to process request");
                }
                if (!this.conn.isOpen()) {
                    // this connection must be opened before it can be used
                    // This has nothing to do with opening a secure tunnel
                    this.conn.open();
                    if (this.conn.isProxied() && this.conn.isSecure() 
                    && !(method instanceof ConnectMethod)) {
                        // we need to create a secure tunnel before we can execute the real method
                        if (!executeConnect()) {
                            // abort, the connect method failed
                            return;
                        }
                    }
                }
                int timeout = 0;
                // see if a timeout is given for this method
                Object param = this.params.getParameter(HttpMethodParams.SO_TIMEOUT);
                if (param == null) {
                    // if not, use the default value
                    param = this.conn.getParams().getParameter(HttpConnectionParams.SO_TIMEOUT);
                }
                if (param != null) {
                    timeout = ((Integer)param).intValue();
                }
                this.conn.setSocketTimeout(timeout);
                
                try {
                    method.execute(state, this.conn);
                    break;
                } catch (HttpRecoverableException httpre) {
                    LOG.debug("Closing the connection.");
                    this.conn.close();
                    LOG.info("Recoverable exception caught when processing request");
                    // update the recoverable exception count.
                    recoverableExceptionCount++;
                
                    // test if this method should be retried                
                    MethodRetryHandler handler = method.getMethodRetryHandler();
                    if (handler == null) {
                        handler = new DefaultMethodRetryHandler();
                    }
                    if (!handler.retryMethod(
                            method, 
                            this.conn, 
                            httpre, 
                            execCount, 
                            requestSent)
                    ) {
                        LOG.warn(
                            "Recoverable exception caught but MethodRetryHandler.retryMethod() "
                            + "returned false, rethrowing exception"
                        );
                        throw httpre;
                    }
                }
            }
        } catch (IOException e) {
            if (this.conn.isOpen()) {
                LOG.debug("Closing the connection.");
                this.conn.close();
            }
            releaseConnection = true;
            throw e;
        } catch (RuntimeException e) {
            if (this.conn.isOpen()) {
                LOG.debug("Closing the connection.");
                this.conn.close();
            }
            releaseConnection = true;
            throw e;
        }
    }
    
    /**
     * Executes a ConnectMethod to establish a tunneled connection.
     * 
     * @return <code>true</code> if the connect was successful
     * 
     * @throws IOException
     * @throws HttpException
     */
    private boolean executeConnect() 
        throws IOException, HttpException {

        this.connectMethod = new ConnectMethod();
        this.connectMethod.getParams().setDefaults(this.params);
        
        int code;
        for (;;) {
            try {
                authenticateProxy(this.connectMethod);
            } catch (AuthenticationException e) {
                LOG.error(e.getMessage(), e);
            }
            executeWithRetry(this.connectMethod);
            code = this.connectMethod.getStatusCode();
            boolean retry = false;
            if (code == HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED) {
                if (processAuthenticationResponse(this.connectMethod)) {
                    retry = true;
                }
            } else {
                this.authProcess = AUTH_NOT_REQUIRED;
            }
            if (!retry) {
                break;
            }
            if (this.connectMethod.getResponseBodyAsStream() != null) {
                this.connectMethod.getResponseBodyAsStream().close();
            }
        }
        if ((code >= 200) && (code < 300)) {
            this.conn.tunnelCreated();
            // Drop the connect method, as it is no longer needed
            this.connectMethod = null;
            return true;
        } else {
            return false;
        }
    }

    /**
     * Fake response
     * @param method
     * @return
     */
    
    private void fakeResponse(final HttpMethod method)
        throws IOException, HttpException {
        // What is to follow is an ugly hack.
        // I REALLY hate having to resort to such
        // an appalling trick
        // The only feasible solution is to split monolithic
        // HttpMethod into HttpRequest/HttpResponse pair.
        // That would allow to execute CONNECT method 
        // behind the scene and return CONNECT HttpResponse 
        // object in response to the original request that 
        // contains the correct status line, headers & 
        // response body.
        LOG.debug("CONNECT failed, fake the response for the original method");
        // Pass the status, headers and response stream to the wrapped
        // method.
        // To ensure that the connection is not released more than once
        // this method is still responsible for releasing the connection. 
        // This will happen when the response body is consumed, or when
        // the wrapped method closes the response connection in 
        // releaseConnection().
        if (method instanceof HttpMethodBase) {
            ((HttpMethodBase) method).fakeResponse(
                this.connectMethod.getStatusLine(),
                this.connectMethod.getResponseHeaderGroup(),
                this.connectMethod.getResponseBodyAsStream()
            );
            method.getProxyAuthState().setAuthScheme(
                this.connectMethod.getProxyAuthState().getAuthScheme());
            this.connectMethod = null;
        } else {
            releaseConnection = true;
            LOG.warn(
                "Unable to fake response on method as it is not derived from HttpMethodBase.");
        }
    }
    
	/**
	 * Process the redirect response.
     * 
	 * @return <code>true</code> if the redirect was successful
	 */
	private boolean processRedirectResponse(final HttpMethod method)
     throws RedirectException  
    {
		//get the location header to find out where to redirect to
		Header locationHeader = method.getResponseHeader("location");
		if (locationHeader == null) {
			// got a redirect response, but no location header
			LOG.error("Received redirect response " + method.getStatusCode()
					+ " but no location header");
			return false;
		}
		String location = locationHeader.getValue();
		if (LOG.isDebugEnabled()) {
			LOG.debug("Redirect requested to location '" + location + "'");
		}
        
		//rfc2616 demands the location value be a complete URI
		//Location       = "Location" ":" absoluteURI
		URI redirectUri = null;
		URI currentUri = null;

		try {
			currentUri = new URI(
				this.conn.getProtocol().getScheme(),
				null,
                this.conn.getHost(), 
                this.conn.getPort(), 
				method.getPath()
			);
			redirectUri = new URI(location, true);
			if (redirectUri.isRelativeURI()) {
				if (this.params.isParameterTrue(HttpClientParams.REJECT_RELATIVE_REDIRECT)) {
					LOG.warn("Relative redirect location '" + location + "' not allowed");
					return false;
				} else { 
					//location is incomplete, use current values for defaults
					LOG.debug("Redirect URI is not absolute - parsing as relative");
					redirectUri = new URI(currentUri, redirectUri);
				}
			}
            method.setURI(redirectUri);
            hostConfiguration.setHost(redirectUri);
		} catch (URIException e) {
			LOG.warn("Redirected location '" + location + "' is malformed");
			return false;
		}

        if (this.params.isParameterFalse(HttpClientParams.ALLOW_CIRCULAR_REDIRECTS)) {
            if (this.redirectLocations == null) {
                this.redirectLocations = new HashSet();
            }
            this.redirectLocations.add(currentUri);
            if (this.redirectLocations.contains(redirectUri)) {
                throw new RedirectException("Circular redirect to '" +
                    redirectUri + "'");   
            }
        }

		if (LOG.isDebugEnabled()) {
			LOG.debug("Redirecting from '" + currentUri.getEscapedURI()
				+ "' to '" + redirectUri.getEscapedURI());
		}

        //And finally invalidate the actual authentication scheme
        method.getHostAuthState().invalidate(); 
		return true;
	}

	/**
	 * Processes a response that requires authentication
	 *
	 * @param method the current {@link HttpMethod HTTP method}
	 *
	 * @return <tt>true</tt> if the authentication challenge can be responsed to,
     *   (that is, at least one of the requested authentication scheme is supported, 
     *   and matching credentials have been found), <tt>false</tt> otherwise.
	 */
	private boolean processAuthenticationResponse(final HttpMethod method) {
		LOG.trace("enter HttpMethodBase.processAuthenticationResponse("
			+ "HttpState, HttpConnection)");

		try {
            switch (method.getStatusCode()) {
                case HttpStatus.SC_UNAUTHORIZED:
                    return processWWWAuthChallenge(method);
                case HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED:
                    return processProxyAuthChallenge(method);
                default:
                    return false;
            }
        } catch (Exception e) {
            if (LOG.isErrorEnabled()) {
                LOG.error(e.getMessage(), e);
            }
            return false;
        }
	}

    private boolean processWWWAuthChallenge(final HttpMethod method)
        throws MalformedChallengeException, AuthenticationException  
    {
        AuthState authstate = method.getHostAuthState();
        if (authstate.isPreemptive()) {
            authstate.invalidate();
        }
        Map challenges = AuthChallengeParser.parseChallenges(
            method.getResponseHeaders(WWW_AUTH_CHALLENGE));
        if (challenges.isEmpty()) {
            return false; 
        }
        AuthScheme authscheme = null;
        try {
            authscheme = this.authProcessor.processChallenge(authstate, challenges);
        } catch (AuthChallengeException e) {
            if (LOG.isWarnEnabled()) {
                LOG.warn(e.getMessage());
            }
        }
        if (authscheme == null) {
            return false;
        }
        String host = conn.getVirtualHost();
        if (host == null) {
            host = conn.getHost();
        }
        int port = conn.getPort();
        HttpAuthRealm realm = new HttpAuthRealm(
            host, port, 
            authscheme.getRealm(), 
            authscheme.getSchemeName());  

        if ((this.authProcess == AUTH_WWW_REQUIRED) && (authscheme.isComplete())) {
            // Already tried and failed
            Credentials credentials = promptForCredentials(
                authscheme, method.getParams(), realm);
            if (credentials == null) {
                if (LOG.isInfoEnabled()) {
                    LOG.info("Failure authenticating with " + realm);
                }
                return false;
            } else {
                return true;
            }
        } else {
            this.authProcess = AUTH_WWW_REQUIRED;
                        
            Credentials credentials = this.state.getCredentials(realm);
            if (credentials == null) {
                credentials = promptForCredentials(
                    authscheme, method.getParams(), realm);
            }
            if (credentials == null) {
                if (LOG.isInfoEnabled()) {
                    LOG.info("No credentials available for the " + realm); 
                }
                return false;
            } else {
                return true;
            }
        }
    }

    private boolean processProxyAuthChallenge(final HttpMethod method)
        throws MalformedChallengeException, AuthenticationException
    {  
        AuthState authstate = method.getProxyAuthState();
        if (authstate.isPreemptive()) {
            authstate.invalidate();
        }
        Map proxyChallenges = AuthChallengeParser.parseChallenges(
            method.getResponseHeaders(PROXY_AUTH_CHALLENGE));
        if (proxyChallenges.isEmpty()) {
            return false; 
        }
        AuthScheme authscheme = null;
        try {
            authscheme = this.authProcessor.processChallenge(authstate, proxyChallenges);
        } catch (AuthChallengeException e) {
            if (LOG.isWarnEnabled()) {
                LOG.warn(e.getMessage());
            }
        }
        if (authscheme == null) {
            return false;
        }
        HttpAuthRealm realm = new HttpAuthRealm(
            conn.getProxyHost(), conn.getProxyPort(), 
            authscheme.getRealm(), 
            authscheme.getSchemeName());  

        if ((this.authProcess == AUTH_PROXY_REQUIRED) && (authscheme.isComplete())) {
            // Already tried and failed
            Credentials credentials = promptForProxyCredentials(
                authscheme, method.getParams(), realm);
            if (credentials == null) {
                if (LOG.isInfoEnabled()) {
                    LOG.info("Failure authenticating with " + realm);
                }
                return false;
            } else {
                return true;
            }
        } else {
            this.authProcess = AUTH_PROXY_REQUIRED;

            Credentials credentials = this.state.getProxyCredentials(realm);
            if (credentials == null) {
                credentials = promptForProxyCredentials(
                    authscheme, method.getParams(), realm);
            }
            if (credentials == null) {
                if (LOG.isInfoEnabled()) {
                    LOG.info("No proxy credentials available for the " + realm); 
                }
                return false;
            } else {
                return true;
            }
        }
    }

    /**
     * Tests if the {@link HttpMethod method} requires a redirect to another location.
     * 
     * @param method HTTP method
     * 
     * @return boolean <tt>true</tt> if a retry is needed, <tt>false</tt> otherwise.
     */
	private boolean isRedirectNeeded(final HttpMethod method) {
		switch (method.getStatusCode()) {
			case HttpStatus.SC_MOVED_TEMPORARILY:
			case HttpStatus.SC_MOVED_PERMANENTLY:
			case HttpStatus.SC_SEE_OTHER:
			case HttpStatus.SC_TEMPORARY_REDIRECT:
				LOG.debug("Redirect required");
                if (method.getFollowRedirects()) {
                    return true;
                } else {
                    LOG.info("Redirect requested but followRedirects is "
                            + "disabled");
                    return false;
                }
			default:
				return false;
		} //end of switch
	}

    /**
     * Tests if the {@link HttpMethod method} requires authentication.
     * 
     * @param method HTTP method
     * 
     * @return boolean <tt>true</tt> if a retry is needed, <tt>false</tt> otherwise.
     */
    private boolean isAuthenticationNeeded(final HttpMethod method) {
        switch (method.getStatusCode()) {
            case HttpStatus.SC_UNAUTHORIZED:
            case HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED:
                LOG.debug("Authorization required");
                if (method.getDoAuthentication()) { //process authentication response
                    return true;
                } else { //let the client handle the authenticaiton
                    LOG.info("Authentication requested but doAuthentication is "
                            + "disabled");
                    return false;
                }
            default:
                return false;
        } //end of switch
    }

    private Credentials promptForCredentials(
        final AuthScheme authScheme,
        final HttpParams params, 
        final HttpAuthRealm realm)
    {
        Credentials creds = null;
        CredentialsProvider credProvider = 
            (CredentialsProvider)params.getParameter(CredentialsProvider.PROVIDER);
        if (credProvider != null) {
            try {
                creds = credProvider.getCredentials(
                    authScheme, realm.getHost(), realm.getPort(), false);
            } catch (CredentialsNotAvailableException e) {
                LOG.warn(e.getMessage());
            }
            if (creds != null) {
                this.state.setCredentials(realm, creds);
                if (LOG.isDebugEnabled()) {
                    LOG.debug("New credentials for " + realm);
                }
            }
        }
        return creds;
    }

    private Credentials promptForProxyCredentials(
        final AuthScheme authScheme,
        final HttpParams params,
        final HttpAuthRealm realm) 
    {
        Credentials creds = null;
        CredentialsProvider credProvider = 
            (CredentialsProvider)params.getParameter(CredentialsProvider.PROVIDER);
        if (credProvider != null) {
            try {
                creds = credProvider.getCredentials(
                    authScheme, realm.getHost(), realm.getPort(), true);
            } catch (CredentialsNotAvailableException e) {
                LOG.warn(e.getMessage());
            }
            if (creds != null) {
                this.state.setProxyCredentials(realm, creds);
                if (LOG.isDebugEnabled()) {
                    LOG.debug("New proxy credentials for " + realm);
                }
            }
        }
        return creds;
    }

    /**
     * @return
     */
    public HostConfiguration getHostConfiguration() {
        return hostConfiguration;
    }

    /**
     * @return
     */
    public HttpState getState() {
        return state;
    }

    /**
     * @return
     */
    public HttpConnectionManager getConnectionManager() {
        return connectionManager;
    }

    /**
     * @return
     */
    public HttpParams getParams() {
        return this.params;
    }
}
