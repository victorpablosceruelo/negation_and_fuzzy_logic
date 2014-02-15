/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpMethodBase.java,v 1.97 2003/01/25 03:28:29 jericho Exp $
 * $Revision: 1.97 $
 * $Date: 2003-01-25 04:28:29 +0100 (Sat, 25 Jan 2003) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "HttpClient", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 * [Additional notices, if required by prior licensing conditions]
 *
 */
package org.apache.commons.httpclient;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.httpclient.cookie.CookiePolicy;
import org.apache.commons.httpclient.cookie.CookieSpec;
import org.apache.commons.httpclient.util.URIUtil;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * <p>
 * An abstract base implementation of {@link HttpMethod}.
 * </p>
 *
 * <p>
 * At minimum, subclasses will need to override
 * </p>
 *
 * <dl>
 * <dt>
 * {@link #getName}
 * </dt>
 * <dd>
 * to return the approriate name for this method
 * </dd>
 * </dl>
 *
 * <p>
 * When a method's request may contain a body, subclasses will typically want
 * to override:
 * </p>
 *
 * <dl>
 * <dt>
 * {@link #getRequestContentLength}
 * </dt>
 * <dd>
 * to indicate the length (in bytes) of that body
 * </dd>
 * <dt>
 * {@link #writeRequestBody writeRequestBody(HttpState,HttpConnection)}
 * </dt>
 * <dd>
 * to write the body
 * </dd>
 * </dl>
 *
 * <p>
 * When a method requires additional request headers, subclasses will typically
 * want to override:
 * </p>
 *
 * <dl>
 * <dt>
 * {@link #addRequestHeaders addRequestHeaders(HttpState,HttpConnection)}
 * </dt>
 * <dd>
 * to write those headers
 * </dd>
 * </dl>
 *
 * <p>
 * When a method expects specific response headers, subclasses may want to
 * override:
 * </p>
 *
 * <dl>
 * <dt>
 * {@link #processResponseHeaders
 * processResponseHeaders(HttpState,HttpConnection)}
 * </dt>
 * <dd>
 * to handle those headers
 * </dd>
 * </dl>
 *
 *
 * @version $Revision: 1.97 $ $Date: 2003-01-25 04:28:29 +0100 (Sat, 25 Jan 2003) $
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Rodney Waldhoff
 * @author Sean C. Sullivan
 * @author <a href="mailto:dion@apache.org">dIon Gillard</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:dims@apache.org">Davanum Srinivas</a>
 * @author Ortwin Glück
 * @author Eric Johnson
 * @author Michael Becke
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 */
public abstract class HttpMethodBase implements HttpMethod {
    //~ Static variables/initializers ··········································

    /** Maximum number of redirects and authentications that will be followed */
    private static int maxForwards = 100;
    // -------------------------------------------------------------- Constants

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(HttpMethod.class);

    /** Log for any wire messages. */
    private static final Log wireLog = LogFactory.getLog("httpclient.wire");

    /** The User-Agent header sent on every request. */
    protected static final Header USER_AGENT;

    static {
        String agent = System.getProperties()
                             .getProperty("httpclient.useragent",
                                          "Jakarta Commons-HttpClient/2.0M1");
        USER_AGENT = new Header("User-Agent", agent);
    }

    //~ Instance variables ·····················································

    /** My request headers, if any. */
    private Map requestHeaders = new HashMap();

    /** The Status-Line from the response. */
    private StatusLine statusLine = null;

    /** My response headers, if any. */
    private Map responseHeaders = new HashMap();

    /** My response footers, if any. */
    private Map responseFooters = null;

    /** Realms that we tried to authenticate to */
    private Set realms = null;

    /** Proxy Realms that we tried to authenticate to */
    private Set proxyRealms = null;

    /** My request path. */
    private String path = null;

    /** My query string, if any. */
    private String queryString = null;

    /** The response body, assuming it has not be intercepted by a sub-class. */
    private InputStream responseStream = null;

    /** The connection that the response stream was read from. */
    private HttpConnection responseConnection = null;

    /** Buffer for the response */
    private byte[] responseBody = null;

    /** Whether or not the request body has been sent. */
    private boolean bodySent = false;

    /** Whether or not I should automatically follow redirects. */
    private boolean followRedirects = false;

    /** Whether or not I should automatically processs authentication. */
    private boolean doAuthentication = true;

    /** Whether or not I should use the HTTP/1.1 protocol. */
    private boolean http11 = true;

    /** True if we're in strict mode. */
    private boolean strictMode = false;

    /** Whether or not I have been executed. */
    private boolean used = false;

    /** How many times did this transparently handle a recoverable exception? */
    private int recoverableExceptionCount = 0;

    /** the host configuration for this method, can be null */
    private HostConfiguration hostConfiguration;

    /**
     * The maximum number of attempts to attempt recovery from an
     * HttpRecoverableException.
     */
    private int maxRetries = 3;

    private boolean inExecute = false;

    private boolean doneWithConnection = false;

    //~ Constructors ···························································

    // ----------------------------------------------------------- Constructors

    /**
     * No-arg constructor.
     */
    public HttpMethodBase() {
    }

    /**
     * Constructor specifying a URI.
     *
     * @param uri either an absolute or relative URI
     * 
     * @throws IllegalArgumentException when URI is invalid
     * @throws IllegalStateException when protocol of the absolute URI is not recognised
     */
    public HttpMethodBase(String uri) {

        try {

            // create a URI and allow for null/empty uri values
            if (uri == null || uri.equals( "" )) {
                uri = "/";
            }
            URI parsedURI = new URI( uri );

            // only set the host if specified by the URI
            if ( parsedURI.isAbsoluteURI() ) {
                hostConfiguration = new HostConfiguration();
                hostConfiguration.setHost(
                    parsedURI.getHost(),
                    parsedURI.getPort(),
                    parsedURI.getScheme()
                ); 
            }
            // else { FIXME: just in case, is not abolsute uri, then?

            // set the path, defaulting to root
            setPath(
                parsedURI.getPath() == null
                ? "/"
                : parsedURI.getPath()
            );
            setQueryString( parsedURI.getEscapedQuery() );

        } catch ( URIException e ) {
            throw new IllegalArgumentException( 
                "Invalid uri '" + uri + "': " + e.getMessage() 
            );
        }
    }

    //~ Methods ································································

    // ------------------------------------------- Property Setters and Getters

    /**
     * Obtain the name of this method, suitable for use in the "request line",
     * for example <tt>GET</tt> or <tt>POST</tt>.
     *
     * @return the name of this method
     */
    public abstract String getName();

    /**
     * @see org.apache.commons.httpclient.HttpMethod#getURI()
     */
    public URI getURI() throws URIException {

        if ( hostConfiguration == null ) {
            // just use a relative URI, the host hasn't been set
            URI tmpUri = new URI(null, null, path, null, null);
            tmpUri.setEscapedQuery(queryString);
            return tmpUri;
        } else {

            // we only want to include the port if it's not the default
            int port = hostConfiguration.getPort();
            if ( port == hostConfiguration.getProtocol().getDefaultPort() ) {
                port = -1;
            }

            URI tmpUri = new URI(
                hostConfiguration.getProtocol().getScheme(),
                null,
                hostConfiguration.getHost(),
                port,
                path,
                null // to set an escaped form
            );
            tmpUri.setEscapedQuery(queryString);
            return tmpUri;

        }

    }

    /**
     * Set whether or not I should automatically follow HTTP redirects (status
     * code 302, etc.)
     *
     * @param followRedirects true to follow redirects, false otherwise
     */
    public void setFollowRedirects(boolean followRedirects) {
        this.followRedirects = followRedirects;
    }

    /**
     * Whether or not I should automatically follow HTTP redirects (status code
     * 302, etc.)
     *
     * @return <tt>true</tt> if I will automatically follow HTTP redirects
     */
    public boolean getFollowRedirects() {
        return this.followRedirects;
    }

    /**
     * Set whether or not I should use the HTTP/1.1 protocol.
     *
     * @param http11 true to use HTTP/1.1, false to use 1.0
     */
    public void setHttp11(boolean http11) {
        this.http11 = http11;
    }

        /**
     * Whether or not I should automatically process responses where
     * authentication is required (status code 401, etc.)
     *
     * @return <tt>true</tt> if authentications will be processed automatically
     * @since 2.0
     */
    public boolean getDoAuthentication() {
        return doAuthentication;
    }

    /**
     * Set whether or not I should automatically process responses where
     * authentication is required (status code 401, etc.)
     *
     * @param doAuthentication <tt>true</tt> to process authentications
     * @since 2.0
     */
    public void setDoAuthentication(boolean doAuthentication) {
        this.doAuthentication = doAuthentication;
    }

    // ---------------------------------------------- Protected Utility Methods

    /**
     * Access to flag to determine if client should use
     * HTTP/1.1 protocol.
     *
     * @return <tt>true</tt> if I should use the HTTP/1.1 protocol
     */
    public boolean isHttp11() {
        return http11;
    }

    /**
     * Set the path part of my request.
     *
     * @param path the path to request
     */
    public void setPath(String path) {
        this.path = path;
    }

    /**
     * Add the specified request header.
     *
     * If a header of the same name already exists, the new value will be
     * appended onto the the existing value list.
     * A <i>header</i> value of <code>null</code> will be ignored.
     * Note that header-name matching is case insensitive.
     *
     * @param header the header to add to the request
     */
    public void addRequestHeader(Header header) {
        log.trace("HttpMethodBase.addRequestHeader(Header)");

        if (header == null) {
          log.debug("null header value ignored");
        } else {
            addRequestHeader(header.getName(), header.getValue());
        }
    }

    /**
     * adds a response footer to the internal list
     */
    public void addResponseFooter(Header footer) {
        if (responseFooters == null) responseFooters = new HashMap();
        responseFooters.put(footer.getName().toLowerCase(), footer);
    }

    /**
     * Get the path part of my request.
     *
     * @return the path to request or "/" if the path is blank.
     */
    public String getPath() {
        return (path == null || path.equals("")) ? "/" : path;
    }

    /**
     * Sets the query string.
     * The user must ensure that the string is properly URL encoded.
     * URIUtil.encodeAll, URIUtil.encodeWithinQuery or URIUtil.encodeQuery can
     * be used to encode parameter names and values.
     * The query string should not start with the question mark character.
     *
     * @param queryString the query string
     */
    public void setQueryString(String queryString) {
        this.queryString = queryString;
    }

    /**
     * Set my query string.
     *
     * @param params an array of {@link NameValuePair}s to add as query string
     *        parameterss
     */
    public void setQueryString(NameValuePair[] params) {
        log.trace("enter HttpMethodBase.setQueryString(NameValuePair[])");
        StringBuffer buf = new StringBuffer();
        boolean needAmp = false;
        for (int i = 0; i < params.length; i++) {
            if (params[i].getName() != null) {
                if (needAmp) {
                    buf.append("&");
                } else {
                    needAmp = true;
                }
                String queryName = null;
                try {
                    queryName = URIUtil.encodeWithinQuery(params[i].getName());
                } catch (URIException urie) {
                    log.error("encoding error within query name", urie);
                    queryName = params[i].getName();
                }
                buf.append(queryName).append("=");
                if (params[i].getValue() != null) {
                    String queryValue = null;
                    try {
                        queryValue =
                            URIUtil.encodeWithinQuery(params[i].getValue());
                    } catch (URIException urie) {
                        log.error("encoding error within query value", urie);
                        queryValue = params[i].getValue();
                    }
                    buf.append(queryValue);
                }
            }
        }
        queryString = buf.toString();
    }

    /**
     * Get my query string.
     *
     * @return The query string portion of the request
     */
    public String getQueryString() {
        return queryString;
    }

    /**
     * Set the specified request header, overwriting any previous value. Note
     * that header-name matching is case-insensitive.
     *
     * @param headerName the header's name
     * @param headerValue the header's value
     */
    public void setRequestHeader(String headerName, String headerValue) {
        Header header = new Header(headerName, headerValue);
        setRequestHeader(header);
    }

    /**
     * Set the specified request header, overwriting any previous value. Note
     * that header-name matching is case insensitive.
     *
     * @param header the header
     */
    public void setRequestHeader(Header header) {
        requestHeaders.put(header.getName().toLowerCase(), header);
    }

    /**
     * Get the request header associated with the given name. Header name
     * matching is case insensitive. <tt>null</tt> will be returned if either
     * <i>headerName</i> is <tt>null</tt> or there is no matching header for
     * <i>headerName</i>.
     *
     * @param headerName the header name to match
     *
     * @return the matching header
     */
    public Header getRequestHeader(String headerName) {
        return (headerName == null)
               ? null : (Header) (requestHeaders.get(headerName.toLowerCase()));
    }

    /**
     * Provides access to the request headers.
     *
     * @return an array of my request headers.
     */
    public Header[] getRequestHeaders() {
        return (Header[]) (requestHeaders.values().toArray(
            new Header[requestHeaders.size()]));
    }

    /**
     * Convenience method top provide access to the status code.
     *
     * @return the status code associated with the latest response.
     */
    public int getStatusCode() {
        return statusLine.getStatusCode();
    }

    /**
     * Provide access to the status line.
     *
     * @return the status line object from the latest response.
     * @since 2.0
     */
    public StatusLine getStatusLine() {
        return statusLine;
    }

    /**
     * Checks if response data is available.
     * @return true if response data is available, false otherwise.
     */
    private boolean responseAvailable() {
        return (responseBody != null) || (responseStream != null);
    }

    /**
     * Provide access to the response headers
     *
     * @return an array of my response headers.
     */
    public Header[] getResponseHeaders() {
        return (Header[]) (responseHeaders.values().toArray(
            new Header[responseHeaders.size()]));
    }

    /**
     * Get the response header associated with the given name. Header name
     * matching is case insensitive. <tt>null</tt> will be returned if either
     * <i>headerName</i> is <tt>null</tt> or there is no matching header for
     * <i>headerName</i>.
     *
     * @param headerName the header name to match
     *
     * @return the matching header
     */
    public Header getResponseHeader(String headerName) {
        return (headerName == null)
               ? null
               : (Header) (responseHeaders.get(headerName.toLowerCase()));
    }

    /**
     * Return my response body, if any, as a byte array.
     * Otherwise return <tt>null</tt>.
     */
    public byte[] getResponseBody() {
        if (responseBody == null) {
            try {
                ByteArrayOutputStream os = new ByteArrayOutputStream();
                InputStream is = getResponseBodyAsStream();
                byte[] buffer = new byte[10000];
                int len;
                while ((len = is.read(buffer)) > 0) {
                    os.write(buffer, 0, len);
                }
                is.close();
                os.close();
                responseBody = os.toByteArray();
                setResponseStream(null);
                log.debug("buffering response body");
            } catch(IOException e) {
                log.error("getResponseBody failed", e);
                responseBody = null;
            }
        }
        return responseBody;
    }

    /**
     * Return my response body, if any, as an {@link InputStream}. Otherwise
     * return <tt>null</tt>.
     *
     * @return the response body as an {@link InputStream}
     *
     * @throws IOException when there are errors obtaining the response
     */
    public InputStream getResponseBodyAsStream() throws IOException {
        if (responseStream != null) {
            return responseStream;
        }
        if (responseBody != null) {
            InputStream byteResponseStream = new ByteArrayInputStream(responseBody);
            log.debug("re-creating response stream from byte array");
            return byteResponseStream;
        }
        return null;
    }

    /**
     * Gets the response body as a string.
     *
     * <b>Note:</b> The string conversion done on the data is done with the
     * default character encoding.  The use of this method may be non-portable.
     *
     * @return my response body, if any, as a {@link String}. Otherwise return
     *         <tt>null</tt>.
     */
    public String getResponseBodyAsString()
    {
        if (!responseAvailable()) 
        {
            return null;
        }
        return HttpConstants.getContentString(getResponseBody(), getResponseCharSet());
    }


    /**
     * Return an array of response footers.
     * @return <tt>null</tt> if no footers are available
     */
    public Header[] getResponseFooters() {
        if (responseFooters == null) {
            return null;
        }
        return (Header[])(responseFooters.values().toArray(
            new Header[responseFooters.size()]));
    }

    /**
     * Get the response footer associated with the given name.
     * Footer name matching is case insensitive.
     * <tt>null</tt> will be returned if either <i>footerName</i> is
     * <tt>null</tt> or there is no matching header for <i>footerName</i>
     * or there are no footers available.
     * @param footerName the footer name to match
     * @return the matching footer
     */
    public Header getResponseFooter(String footerName) {
        if (responseFooters == null) {
            return null;
        }
        return (footerName == null) ? null :
            (Header)(responseFooters.get(footerName.toLowerCase()));
    }


    protected void setResponseStream(InputStream responseStream) {
        this.responseStream = responseStream;
    }

    /**
     * Provide access to the status text
     *
     * @return the status text (or "reason phrase") associated with the latest
     *         response.
     */
    public String getStatusText() {
        return statusLine.getReasonPhrase();
    }

    /**
     * Turns strict mode on or off.  In strict mode (the default) we following
     * the letter of RFC 2616, the Http 1.1 specification. If strict mode is
     * turned off we attempt to violate the specification in the same way that
     * most Http user agent's do (and many HTTP servers expect. NOTE:
     * StrictMode is currently experimental and its functionlaity may change
     * in the future.
     *
     * @param strictMode true for strict mode, false otherwise
     */
    public void setStrictMode(boolean strictMode) {
        this.strictMode = strictMode;
    }

    /**
     * Returns the value of strictMode. NOTE:  StrictMode is currently
     * experimental and its functionlaity may  change in the future.
     *
     * @return true if strict mode is enabled.
     */
    public boolean isStrictMode() {
        return strictMode;
    }

    /**
     * Add the specified request header, NOT overwriting any previous value.
     * Note that header-name matching is case insensitive.
     *
     * @param headerName the header's name
     * @param headerValue the header's value
     */
    public void addRequestHeader(String headerName, String headerValue) {
        // "It must be possible to combine the multiple header fields into
        // one "field-name: field-value" pair, without changing the
        // semantics of the message, by appending each subsequent field-value
        // to the first, each separated by a comma."
        //   - HTTP/1.0 (4.3)
        Header header = getRequestHeader(headerName);
        if (null == header) {
            // header doesn't exist already, simply create with name and value
            header = new Header(headerName, headerValue);
        } else {
            // header exists, add this value to the comma separated list
            header.setValue(getNewHeaderValue(header, headerValue));
        }
        setRequestHeader(header);
    }

    protected boolean shouldCloseConnection() {
        if (!http11) {
            if (getName().equals(ConnectMethod.NAME) &&
                    (statusLine.getStatusCode() == HttpStatus.SC_OK)) {
                log.debug("Will leave connection open for tunneling");
                return false;
            } else {
                log.debug("Should close connection since using HTTP/1.0, " +
                        "ConnectMethod and status is OK");
                return true;
            }
        } else {
            Header connectionHeader = getResponseHeader("connection");
            if (null != connectionHeader
                && "close".equalsIgnoreCase(connectionHeader.getValue())) {
                log.debug("Should close connection since \"Connection: close\" header found.");
                return true;
            }
        }
        return false;
    }

    private boolean isRetryNeeded(int statusCode, HttpState state, HttpConnection conn) {
        switch (statusCode) {
            case HttpStatus.SC_UNAUTHORIZED:
            case HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED:
                log.debug("Authorization required");
                if (doAuthentication) { //process authentication response
                    //if the authentication is successful, return the statusCode
                    //otherwise, drop through the switch and try again.
                    if (processAuthenticationResponse(state)) {
                        return false;
                    }
                } else { //let the client handle the authenticaiton
                    return false;
                }
                break;

            case HttpStatus.SC_MOVED_TEMPORARILY:
            case HttpStatus.SC_MOVED_PERMANENTLY:
            case HttpStatus.SC_TEMPORARY_REDIRECT:
                log.debug("Redirect required");

                if (! processRedirectResponse(conn)) {
                    return false;
                }
                break;

            default:
                // neither an unauthorized nor a redirect response
                return false;
        } //end of switch

        return true;
    }

    private void checkExecuteConditions(HttpState state, HttpConnection conn)
    throws HttpException {

        if (null == state) {
            throw new NullPointerException("HttpState parameter");
        }
        if (null == conn) {
            throw new NullPointerException("HttpConnection parameter");
        }
        if (hasBeenUsed()) {
            throw new HttpException("Already used, but not recycled.");
        }
        if (!validate()) {
            throw new HttpException("Not valid");
        }

        if (inExecute) {
            throw new IllegalStateException("Execute invoked recursively, or exited abnormally.");
        }
    }

    /**
     * Execute this method. Note that we cannot currently support redirects
     * that change  the connection parameters (host, port, protocol) because
     * we  don't yet have a good way to get the new connection.  For  the time
     * being, we just return the redirect response code,  and allow the user
     * agent to resubmit if desired.
     *
     * @param state {@link HttpState} information to associate with this
     *        request. Must be non-null.
     * @param conn the{@link HttpConnection} to write to/read from. Must be
     *        non-null. Note that we cannot currently support redirects that
     *        change the HttpConnection parameters (host, port, protocol)
     *        because we don't yet have a good way to get the new connection.
     *        For the time being, we just return the 302 response, and allow
     *        the user agent to resubmit if desired.
     *
     * @return the integer status code if one was obtained, or <tt>-1</tt>
     *
     * @throws HttpException  if an protocol exception occurs
     * @throws HttpRecoverableException if too many redirects occur.
     * @throws IOException if an I/O error occurs
     * @throws NullPointerException if the state is null
     */
    public int execute(HttpState state, HttpConnection conn)
    throws HttpException, IOException, NullPointerException {
        log.trace("enter HttpMethodBase.execute(HttpState, HttpConnection)");

        checkExecuteConditions(state, conn);
        inExecute = true;

        try {
            //TODO: This method is too large

            //pre-emptively add the authorization header, if required.
            Authenticator.authenticate(this, state);
            if (conn.isProxied()) {
                Authenticator.authenticateProxy(this, state);
            }

            //Set visited = new HashSet();
            realms = new HashSet();
            proxyRealms = new HashSet();
            int forwardCount = 0; //protect from an infinite loop

            while (forwardCount++ < maxForwards) {
                // on every retry, reset this state information.
                responseConnection = conn;
                conn.setLastResponseInputStream(null);

                if (log.isDebugEnabled()) {
                    log.debug("Execute loop try " + forwardCount);
                }

                //write the request and read the response, will retry
                processRequest(state, conn);

                //if SC_CONTINUE write the request body
                writeRemainingRequestBody(state, conn);

                if (!isRetryNeeded(statusLine.getStatusCode(), state, conn)) {
                    // nope, no retry needed, exit loop.
                    break;
                }
                /*
                   Revisiting may be desired. We do not know about the server's internal state.

                //check to see if we have visited this url before
                if (visited.contains(generateVisitedKey(conn))) {
                    log.error("Link " + generateVisitedKey(conn) + "' revisited");
                    return statusCode;
                }
                visited.add(generateVisitedKey(conn));
                */

                // retry - close previous stream.  Caution - this causes
                // responseBodyConsumed to be called, which may also close the
                // connection.
                if (responseStream != null) {
                    responseStream.close();
                }

            } //end of retry loop

            if (forwardCount >= maxForwards) {
                log.error("Narrowly avoided an infinite loop in execute");
                throw new HttpRecoverableException("Maximum redirects ("+ maxForwards +") exceeded");
            }
        }
        finally {
            inExecute = false;
            // If the response has been fully processed, return the connection
            // to the pool.  Use this flag, rather than other tests (like
            // responseStream == null), as subclasses, might reset the stream,
            // for example, reading the entire response into a file and then
            // setting the file as the stream.
            if (doneWithConnection) {
                ensureConnectionRelease();
            }
        }

        return statusLine.getStatusCode();
    }

    private boolean processRedirectResponse(HttpConnection conn) {

        if (!getFollowRedirects()) {
            log.info("Redirect requested but followRedirects is "
                    + "disabled");
            return false;
        }

        //get the location header to find out where to redirect to
        Header locationHeader = getResponseHeader("location");
        if (locationHeader == null) {
            // got a redirect response, but no location header
            log.error("Received redirect response " + getStatusCode()
                    + " but no location header");
            return false;
        }
        String location = locationHeader.getValue();
        if (log.isDebugEnabled()) {
            log.debug("Redirect requested to location '" + location
                    + "'");
        }

        //rfc2616 demands the location value be a complete URI
        //Location       = "Location" ":" absoluteURI
        URL redirectUrl = null;
        URL currentUrl = null;

        try {
            currentUrl = new URL(conn.getProtocol().getScheme(),
                conn.getHost(), conn.getPort(), "");
            redirectUrl = new URL(location);
        } catch (MalformedURLException e) {
            if (isStrictMode()) {
                log.warn("Redirected location '" + location +
                        "' is not acceptable in strict mode");
                return false;
            } else { //location is incomplete, use current values for defaults
                try {
                    log.debug("Redirect URL is not absolute - parsing as relative");
                    redirectUrl = new URL(currentUrl, location);
                } catch (MalformedURLException ex) {
                    log.warn("Redirected location '" + location
                            + "' is malformed");
                    return false;
                }
            }
        }

        //check for redirect to a different protocol, host or port
        try{
            checkValidRedirect(currentUrl, redirectUrl);
        } catch (HttpException ex) {
            //log the error and let the client handle the redirect
            log.warn(ex.getMessage());
            return false;
        }

        //update the current location with the redirect location.
        //avoiding use of URL.getPath() and URL.getQuery() to keep
        //jdk1.2 comliance.
        setPath( URIUtil.getPath( redirectUrl.toString() ) );
        setQueryString( URIUtil.getQuery( redirectUrl.toString() ) );

        if (log.isDebugEnabled()) {
            log.debug("Redirecting from '" + currentUrl.toExternalForm()
                    + "' to '" + redirectUrl.toExternalForm());
        }

        return true;

    }


    /**
     * Check for a valid redirect given the current conn and new url.
     * Redirect to a different protocol, host or port are checked for validity.
     *
     * @param currentUrl The current URL (redirecting from)
     * @param redirectUrl The new URL to redirect to
     * @throws HttpException if the redirect is invalid
     * @since 2.0
     */
    private static void checkValidRedirect(URL currentUrl, URL redirectUrl)
    throws HttpException {
        log.trace("enter HttpMethodBase.checkValidRedirect(HttpConnection, URL)");

        String oldProtocol = currentUrl.getProtocol();
        String newProtocol = redirectUrl.getProtocol();
        if (! oldProtocol.equals(newProtocol)) {
            throw new HttpException("Redirect from protocol " + oldProtocol
                    + " to " + newProtocol + " is not supported");
        }

        String oldHost = currentUrl.getHost();
        String newHost = redirectUrl.getHost();
        if (! oldHost.equalsIgnoreCase(newHost)) {
            throw new HttpException("Redirect from host " + oldHost
                    + " to " + newHost + " is not supported");
        }

        int oldPort = currentUrl.getPort();
        if (oldPort < 0) {
            oldPort = getDefaultPort(oldProtocol);
        }
        int newPort = redirectUrl.getPort();
        if (newPort < 0) {
            newPort = getDefaultPort(newProtocol);
        }
        if (oldPort != newPort) {
            throw new HttpException("Redirect from port " + oldPort
                    + " to " + newPort + " is not supported");
        }
    }


    /**
     * Returns the default port for the given protocol.
     *
     * @param protocol currently only http and https are recognized
     * @return the default port of the given protocol or -1 if the
     * protocol is not recognized.
     *
     * @since 2.0
     *
     */
    private static int getDefaultPort(String protocol) {
        String proto = protocol.toLowerCase().trim();
        if (proto.equals("http")){
            return 80;
        }
        else if (proto.equals("https")){
            return 443;
        }
        return -1;
    }

    /**
     * Whether the object has been used and not recycled.
     *
     * @return <tt>true</tt> if I have been {@link #execute executed} but not
     *         recycled.
     */
    public boolean hasBeenUsed() {
        return used;
    }

    /**
     * Recycle this method so that it can be used again. All of my instances
     * variables will be reset once this method has been called.
     */
    public void recycle() {
        log.trace("enter HttpMethodBase.recycle()");

        releaseConnection();

        path = null;
        followRedirects = false;
        doAuthentication = true;
        queryString = null;
        requestHeaders.clear();
        responseHeaders.clear();
        statusLine = null;
        used = false;
        http11 = true;
        bodySent = false;
        responseBody = null;
        recoverableExceptionCount = 0;
        inExecute = false;
        doneWithConnection = false;
    }

    /**
     * @see org.apache.commons.httpclient.HttpMethod#releaseConnection()
     *
     * @since 2.0
     */
    public void releaseConnection() {

        if (responseStream != null) {
            try {
                // FYI - this may indirectly invoke responseBodyConsumed.
                responseStream.close();
            } catch (IOException e) {
                // attempting cleanup, don't care about exception.
            }
        }

    }

    /**
     * Remove the request header associated with the given name. Note that
     * header-name matching is case insensitive.
     *
     * @param headerName the header name
     */
    public void removeRequestHeader(String headerName) {
        requestHeaders.remove(headerName.toLowerCase());
    }

    // ---------------------------------------------------------------- Queries

    /**
     * Confirm that I am ready to execute.
     *
     * <p>
     * This implementation always returns <tt>true</tt>.
     * </p>
     *
     * @return <tt>true</tt>
     */
    public boolean validate() {
        return true;
    }

    /**
     * Return the length (in bytes) of my request body, suitable for use in a
     * <tt>Content-Length</tt> header.
     *
     * <p>
     * Return <tt>-1</tt> when the content-length is unknown.
     * </p>
     *
     * <p>
     * This implementation returns <tt>0</tt>, indicating that the request has
     * no body.
     * </p>
     *
     * @return <tt>0</tt>, indicating that the request has no body.
     */
    protected int getRequestContentLength() {
        return 0;
    }

    /**
     * Adds an <tt>Authorization</tt> request if needed, as long as no
     * <tt>Authorization</tt> request header already exists.
     *
     * @param state current state of http requests
     * @param conn the connection to use for I/O
     *
     * @throws IOException when errors occur reading or writing to/from the
     *         connection
     * @throws HttpException when a recoverable error occurs
     */
    protected void addAuthorizationRequestHeader(HttpState state,
                                                 HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter HttpMethodBase.addAuthorizationRequestHeader("
                  + "HttpState, HttpConnection)");

        // add authorization header, if needed
        if (getRequestHeader(Authenticator.WWW_AUTH_RESP) == null) {
            Header wwwAuthenticateHeader = getResponseHeader(
                                               Authenticator.WWW_AUTH);
            if (null != wwwAuthenticateHeader) {
                try {
                    Authenticator.authenticate(this, state);
                } catch (HttpException e) {
                    // ignored
                }
            }
        }
    }

    /**
     * Adds a <tt>Content-Length</tt> or <tt>Transfer-Encoding: Chunked</tt>
     * request header, as long as no <tt>Content-Length</tt> request header
     * already exists.
     *
     * @param state current state of http requests
     * @param conn the connection to use for I/O
     *
     * @throws IOException when errors occur reading or writing to/from the
     *         connection
     * @throws HttpException when a recoverable error occurs
     */
    protected void addContentLengthRequestHeader(HttpState state,
                                                 HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter HttpMethodBase.addContentLengthRequestHeader("
                  + "HttpState, HttpConnection)");

        // add content length or chunking
        int len = getRequestContentLength();
        if (getRequestHeader("content-length") == null) {
            if (0 < len) {
                setRequestHeader("Content-Length", String.valueOf(len));
            } else if (http11 && (len < 0)) {
                setRequestHeader("Transfer-Encoding", "chunked");
            }
        }
    }

    /**
     * Adds a <tt>Cookie</tt> request containing the matching {@link Cookie}s.
     *
     * @param state current state of http requests
     * @param conn the connection to use for I/O
     *
     * @throws IOException when errors occur reading or writing to/from the
     *         connection
     * @throws HttpException when a recoverable error occurs
     */
    protected void addCookieRequestHeader(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter HttpMethodBase.addCookieRequestHeader(HttpState, "
                  + "HttpConnection)");

        CookieSpec matcher = CookiePolicy.getSpecByPolicy(state.getCookiePolicy());
        Cookie[] cookies = matcher.match(
          conn.getHost(),
          conn.getPort(),
          getPath(),
          conn.isSecure(),
          state.getCookies());
        if ((cookies != null) && (cookies.length > 0))
        {
            setRequestHeader(matcher.formatCookieHeader(cookies));
        }
    }

    /**
     * Adds a <tt>Host</tt> request header, as long as no <tt>Host</tt> request
     * header already exists.
     *
     * @param state current state of http requests
     * @param conn the connection to use for I/O
     *
     * @throws IOException when errors occur reading or writing to/from the
     *         connection
     * @throws HttpException when a recoverable error occurs
     */
    protected void addHostRequestHeader(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter HttpMethodBase.addHostRequestHeader(HttpState, "
                  + "HttpConnection)");

        // Per 19.6.1.1 of RFC 2616, it is legal for HTTP/1.0 based
        // applications to send the Host request-header.
        // TODO: Add the ability to disable the sending of this header for
        //       HTTP/1.0 requests.
        String host = conn.getHost();
        int port = conn.getPort();

        if (getRequestHeader("host") != null) {
            log.debug(
                "Request to add Host header ignored: header already added");
            return;
        }

        // Note: RFC 2616 uses the term "internet host name" for what goes on the
        // host line.  It would seem to imply that host should be blank if the
        // host is a number instead of an name.  Based on the behavior of web
        // browsers, and the fact that RFC 2616 never defines the phrase "internet
        // host name", and the bad behavior of HttpClient that follows if we
        // send blank, I interpret this as a small misstatement in the RFC, where
        // they meant to say "internet host".  So IP numbers get sent as host
        // entries too. -- Eric Johnson 12/13/2002
        if (log.isDebugEnabled()) {
            log.debug("Adding Host request header");
        }

        //appends the port only if not using the default port for the protocol
        if ( conn.getProtocol().getDefaultPort() != port ) {
            host += (":" + port);
        }

        setRequestHeader("Host", host);
    }

    /**
     * Adds a <tt>Proxy-Authorization</tt> request if needed, as long as no
     * <tt>Proxy-Authorization</tt> request header already exists.
     *
     * @param state current state of http requests
     * @param conn the connection to use for I/O
     *
     * @throws IOException when errors occur reading or writing to/from the
     *         connection
     * @throws HttpException when a recoverable error occurs
     */
    protected void addProxyAuthorizationRequestHeader(HttpState state,
                                                      HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter HttpMethodBase.addProxyAuthorizationRequestHeader("
                  + "HttpState, HttpConnection)");

        // add proxy authorization header, if needed
        if (getRequestHeader(Authenticator.PROXY_AUTH_RESP) == null) {
            Header wwwAuthenticateHeader = getResponseHeader(
                                               Authenticator.PROXY_AUTH);
            if (null != wwwAuthenticateHeader) {
                try {
                    Authenticator.authenticateProxy(this, state);
                } catch (HttpException e) {
                    // ignored
                }
            }
        }
    }

    /**
     * Populates the request headers map to with additional {@link Header
     * headers} to be submitted to the given {@link HttpConnection}.
     *
     * <p>
     * This implementation adds <tt>User-Agent</tt>, <tt>Host</tt>,
     * <tt>Cookie</tt>, <tt>Content-Length</tt>, <tt>Transfer-Encoding</tt>,
     * and <tt>Authorization</tt> headers, when appropriate.
     * </p>
     *
     * <p>
     * Subclasses may want to override this method to to add additional
     * headers, and may choose to invoke this implementation (via
     * <tt>super</tt>) to add the "standard" headers.
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} the headers will eventually be
     *        written to
     * @throws IOException when an error occurs writing the request
     * @throws HttpException when a HTTP protocol error occurs
     *
     * @see #writeRequestHeaders
     */
    protected void addRequestHeaders(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter HttpMethodBase.addRequestHeaders(HttpState, "
            + "HttpConnection)");

        addUserAgentRequestHeader(state, conn);
        addHostRequestHeader(state, conn);
        addCookieRequestHeader(state, conn);
        addAuthorizationRequestHeader(state, conn);
        addProxyAuthorizationRequestHeader(state, conn);
        addContentLengthRequestHeader(state, conn);
    }

    /**
     * Adds a default <tt>User-Agent</tt> request header, as long as no
     * <tt>User-Agent</tt> request header already exists.
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} the headers will eventually be
     *        written to
     * @throws IOException when an error occurs writing the request
     * @throws HttpException when a HTTP protocol error occurs
     */
    protected void addUserAgentRequestHeader(HttpState state,
                                             HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter HttpMethodBase.addUserAgentRequestHeaders(HttpState, "
            + "HttpConnection)");

        if (getRequestHeader("user-agent") == null) {
            setRequestHeader(HttpMethodBase.USER_AGENT);
        }
    }

    /**
     * Throws an {@link IllegalStateException} if used but not recycled.
     *
     * @throws IllegalStateException if the method has been used and not
     *      recycled
     */
    protected void checkNotUsed() throws IllegalStateException {
        if (used) {
            throw new IllegalStateException("Already used.");
        }
    }

    /**
     * Throws an {@link IllegalStateException} if not used since last recycle.
     *
     * @throws IllegalStateException if not used
     */
    protected void checkUsed()  throws IllegalStateException {
        if (!used) {
            throw new IllegalStateException("Not Used.");
        }
    }

    // ------------------------------------------------- Static Utility Methods

    /**
     * Generate an HTTP/S request line according to the specified attributes.
     *
     * @param connection the connection the request will be sent to
     * @param name the method name generate a request for
     * @param requestPath the path string for the request
     * @param query the query string for the request
     * @param protocol the protocol to use (e.g. HTTP/1.0)
     *
     * @return a line to send to the server that will fulfil the request
     */
    protected static String generateRequestLine(HttpConnection connection,
        String name, String requestPath, String query, String protocol) {
        log.trace("enter HttpMethodBase.generateRequestLine(HttpConnection, "
            + "String, String, String, String)");

        StringBuffer buf = new StringBuffer();
        String path = null;
        try {
            path = (requestPath == null) ? "/" : URIUtil.encodePath(requestPath);
        } catch (URIException urie) {
            log.error("URI path encoding error");
            path = requestPath;
        }
        buf.append(path);
        if (query != null) {
            if (query.indexOf("?") != 0) {
                buf.append("?");
            }
            String queryString = null;
            queryString = (query == null) ? "/" : query;
            buf.append(queryString);
        }

        if (!connection.isProxied() || connection.isTransparent()) {
            return (name + " " + buf.toString() + " " + protocol + "\r\n");
        } else {
            if (connection.isSecure()) {
                return (name + " https://" + connection.getHost()
                       + ((443 == connection.getPort()
                               || -1 == connection.getPort())
                          ? "" : (":" + connection.getPort())) + buf.toString()
                       + " " + protocol + "\r\n");
            } else {
                return (name + " http://" + connection.getHost()
                       + ((80 == connection.getPort()
                               || -1 == connection.getPort())
                          ? "" : (":" + connection.getPort())) + buf.toString()
                       + " " + protocol + "\r\n");
            }
        }
    }

    /**
     * When this method is invoked, {@link #readResponseBody
     * readResponseBody(HttpState,HttpConnection)} will have been invoked.
     *
     * <p>
     * This implementation does nothing.
     * </p>
     *
     * <p>
     * Subclasses may want to override this method.
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to read the response from
     *
     * @see #readResponse
     * @see #readResponseBody
     */
    protected void processResponseBody(HttpState state, HttpConnection conn) {
    }

    /**
     * When this method is invoked, the response headers map will have been
     * populated with the response headers (in other words, {@link
     * #readResponseHeaders readResponseHeaders(HttpState,HttpConnection)}
     * will have been invoked).
     *
     * <p>
     * This implementation will handle the <tt>Set-Cookie</tt> and
     * <tt>Set-Cookie2</tt> headers, if any, adding the relevant cookies to
     * the given {@link HttpState}.
     * </p>
     *
     * <p>
     * Subclasses may want to override this method to specially process
     * additional headers, and/or invoke this method (via <tt>super</tt>) to
     * process the <tt>Set-Cookie</tt> and <tt>Set-Cookie2</tt> headers.
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to read the response from
     *
     * @see #readResponse
     * @see #readResponseHeaders
     */
    protected void processResponseHeaders(HttpState state,
        HttpConnection conn) {
        log.trace("enter HttpMethodBase.processResponseHeaders(HttpState, "
            + "HttpConnection)");

        // add cookies, if any
        // should we set cookies?
        Header setCookieHeader = getResponseHeader("set-cookie2");
        if (null == setCookieHeader) { //ignore old-style if new is supported
            setCookieHeader = getResponseHeader("set-cookie");
        }

        if (setCookieHeader == null) return;
        try {

            CookieSpec parser = CookiePolicy.getSpecByPolicy(state.getCookiePolicy());
            Cookie[] cookies = parser.parse(
              conn.getHost(),
              conn.getPort(),
              getPath(),
              conn.isSecure(),
              setCookieHeader);
            for (int i = 0; i < cookies.length; i++)
            {
                Cookie cookie = cookies[i];
                parser.validate(
                  conn.getHost(),
                  conn.getPort(),
                  getPath(),
                  conn.isSecure(),
                  cookie);
                if (log.isDebugEnabled()) {
                    log.debug("Cookie accepted: \"" + parser.formatCookie(cookie) + "\"");
                }
                state.addCookie(cookie);
            }

        } catch (HttpException e) {
            if (log.isWarnEnabled()) {
                log.warn("Cookie rejected: \"" + setCookieHeader.getValue() + "\". " + e.getMessage());
            }
        }
    }

    /**
     * When this method is invoked, the {@link #getStatusCode status code} and
     * {@link #getStatusText status text} values will have been set (in other
     * words, {@link #readStatusLine readStatusLine(HttpState,HttpConnection}
     * will have been invoked).
     *
     * <p>
     * Subclasses may want to override this method to respond to these value.
     * This implementation does nothing.
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to read the response from
     *
     * @see #readResponse
     * @see #readStatusLine
     */
    protected void processStatusLine(HttpState state, HttpConnection conn) {
    }

    /**
     * Reads the response from the given {@link HttpConnection}.
     *
     * <p>
     * The response is written according to the following logic:
     *
     * <ol>
     * <li>
     * {@link #readStatusLine readStatusLine(HttpState,HttpConnection)} is
     * invoked to read the request line.
     * </li>
     * <li>
     * {@link #processStatusLine processStatusLine(HttpState,HttpConnection)}
     * is invoked, allowing the method to respond to the status line if
     * desired.
     * </li>
     * <li>
     * {@link #readResponseHeaders
     * readResponseHeaders(HttpState,HttpConnection} is invoked to read the
     * associated headers.
     * </li>
     * <li>
     * {@link #processResponseHeaders
     * processResponseHeaders(HttpState,HttpConnection} is invoked, allowing
     * the method to respond to the headers if desired.
     * </li>
     * <li>
     * {@link #readResponseBody readResponseBody(HttpState,HttpConnection)} is
     * invoked to read the associated body (if any).
     * </li>
     * <li>
     * {@link #processResponseBody
     * processResponseBody(HttpState,HttpConnection} is invoked, allowing the
     * method to respond to the body if desired.
     * </li>
     * </ol>
     *
     * Subclasses may want to override one or more of the above methods to to
     * customize the processing. (Or they may choose to override this method
     * if dramatically different processing is required.)
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to read the response from
     * @throws HttpException when a protocol or i/o error occurs or state is invalid
     */
    protected void readResponse(HttpState state, HttpConnection conn)
    throws HttpException {
        log.trace(
            "enter HttpMethodBase.readResponse(HttpState, HttpConnection)");
        try {
            readStatusLine(state, conn);
            processStatusLine(state, conn);
            readResponseHeaders(state, conn);
            processResponseHeaders(state, conn);
            readResponseBody(state, conn);
            processResponseBody(state, conn);
        } catch(IOException e) {
            throw new HttpRecoverableException(e.toString());
        }
    }

    /**
     * Read the response body from the given {@link HttpConnection}.
     *
     * <p>
     * The current implementation wraps the socket level stream with
     * an appropriate stream for the type of response (chunked, content-length,
     * or auto-close).  If there is no response body, the connection associated
     * with the request will be returned to the connection manager.
     * </p>
     *
     * <p>
     * Subclasses may want to override this method to to customize the
     * processing.
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to read the response from
     * @throws IOException when i/o errors occur reading the response
     * @throws HttpException when a protocol error occurs or state is invalid
     *
     * @see #readResponse
     * @see #processResponseBody
     */
    protected void readResponseBody(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace(
            "enter HttpMethodBase.readResponseBody(HttpState, HttpConnection)");

        // assume we are not done with the connection if we get a stream
        doneWithConnection = false;
        InputStream stream = _readResponseBody(conn);
        if (stream == null) {
            // done using the connection!
            responseBodyConsumed();
        }
        else {
            conn.setLastResponseInputStream(stream);
            setResponseStream(stream);
        }
    }

    /**
     * Read the response body from the given {@link HttpConnection}.
     * <p>
     * The current implementation returns an appropriate stream
     * (according to the values of the
     * <tt>Content-Length</tt> and <tt>Transfer-Encoding</tt>
     * headers, if any).
     * <p>
     *
     * @see #readResponse
     * @see #processResponseBody
     *
     * @param conn the {@link HttpConnection} to read the response from
     * @return InputStream to read the response body from
     */
    private InputStream _readResponseBody(HttpConnection conn)
    throws IOException {
        log.trace("enter HttpMethodBase.readResponseBody(HttpState, HttpConnection)");

        responseBody = null; // is this desired?
        Header lengthHeader = getResponseHeader("Content-Length");
        Header transferEncodingHeader = getResponseHeader("Transfer-Encoding");
        InputStream is = conn.getResponseInputStream();
        if (wireLog.isDebugEnabled()) {
            is = new WireLogInputStream(is);
        }
        InputStream result = null;
        // We use Transfer-Encoding if present and ignore Content-Length.
        // RFC2616, 4.4 item number 3
        if (null != transferEncodingHeader) {
            if ("chunked".equalsIgnoreCase(transferEncodingHeader.getValue())) {
                result = new ChunkedInputStream(is, this);
            }
        } else if (null != lengthHeader) {

            // we're using this just in case the content length is duplicated
            // i.e. '57, 57'
            HeaderElement[] lengthElements = lengthHeader.getValues();
            String lengthValue = null;

            if ( lengthElements.length > 1 ) {
                // looks like the content length header was duplicated. if so
                // they won't be key=value pairs so we just want to get
                // the name not the value (which should be null)
                // take the first value and ignore any others
                lengthValue = lengthElements[0].getName();
            } else {
                lengthValue = lengthHeader.getValue();
            }

            try {
                int expectedLength = Integer.parseInt(lengthValue);
                // FIXME: what if the content length is 0, perhaps we should
                // just return an empty stream in that case
                result = new ContentLengthInputStream(is, expectedLength);
            } catch(NumberFormatException e) {
                throw new HttpException(
                    "Unable to parse server response content length: '"
                    + lengthValue + "'"
                );

            }

        } else if(canResponseHaveBody(statusLine.getStatusCode())
                && !getName().equals(ConnectMethod.NAME)){
            result = is;
        }

        // if there is a result - ALWAYS wrap it in an observer which will
        // close the underlying stream as soon as it is consumed, and notify
        // the watcher that the stream has been consumed.
        if (result != null) {
            result = new AutoCloseInputStream(result, m_responseWatcher);
        }

        return result;
    }

    /**
     * Read response headers from the given {@link HttpConnection}, populating
     * the response headers map.
     *
     * <p>
     * Subclasses may want to override this method to to customize the
     * processing.
     * </p>
     *
     * <p>
     * "It must be possible to combine the multiple header fields into one
     * "field-name: field-value" pair, without changing the semantics of the
     * message, by appending each subsequent field-value to the first, each
     * separated by a comma." - HTTP/1.0 (4.3)
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to read the response from
     * @throws IOException when i/o errors occur reading the response
     * @throws HttpException when a protocol error occurs or state is invalid
     *
     * @see #readResponse
     * @see #processResponseHeaders
     */
    protected void readResponseHeaders(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter HttpMethodBase.readResponseHeaders(HttpState,"
            + "HttpConnection)");

        responseHeaders.clear();

        String name = null;
        String value = null;
        for (; ;) {
            String line = conn.readLine();
            if ((line == null) || (line.length() < 1)) {
                break;
            }

            // Parse the header name and value
            // Check for folded headers first
            // Detect LWS-char see HTTP/1.0 or HTTP/1.1 Section 2.2
            // discussion on folded headers
            boolean isFolded = false;
            if ((line.charAt(0) == ' ') || (line.charAt(0) == '\t')) {
                // we have continuation folded header
                // so append value
                isFolded = true;
                value = line.substring(1).trim();
            } else {
                // Otherwise we should have normal HTTP header line
                // Parse the header name and value
                int colon = line.indexOf(":");
                if (colon < 0) {
                    throw new HttpException("Unable to parse header: " + line);
                }
                name = line.substring(0, colon).trim();
                value = line.substring(colon + 1).trim();
            }
            Header header = getResponseHeader(name);
            if (null == header) {
                header = new Header(name, value);
            } else {
                String oldvalue = header.getValue();
                if (null != oldvalue) {
                    if (isFolded) {
                        // LWS becomes space plus extended value
                        header = new Header(name, oldvalue + " " + value);
                    } else {
                        // Append additional header value
                        header = new Header(name, oldvalue + ", " + value);
                    }
                } else {
                    header = new Header(name, value);
                }
            }
            setResponseHeader(header);
        }
    }

    /**
     * Read the status line from the given {@link HttpConnection}, setting my
     * {@link #getStatusCode status code} and {@link #getStatusText status
     * text}.
     *
     * <p>
     * Subclasses may want to override this method to to customize the
     * processing.
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to read the response from
     *
     * @throws IOException when errors occur reading the status line
     * @throws HttpException If there is no status line, the protocol is not
     *      recognised, if we are unable to parse the status code from the line,
     *      or there was no status text
     * @throws HttpRecoverableException when the status line is null and the
     *      request should be retried
     *
     * @see StatusLine
     */
    protected void readStatusLine(HttpState state, HttpConnection conn)
    throws IOException, HttpRecoverableException, HttpException {
        log.trace(
            "enter HttpMethodBase.readStatusLine(HttpState, HttpConnection)");

        //read out the HTTP status string
        String statusString = conn.readLine();
        while ((statusString != null) && !statusString.startsWith("HTTP/")) {
            statusString = conn.readLine();
        }
        if (statusString == null) {
            // A null statusString means the connection was lost before we got a
            // response.  Try again.
            throw new HttpRecoverableException("Error in parsing the status "
                + " line from the response: unable to find line starting with"
                + " \"HTTP/\"");
        }

        //create the status line from the status string
        statusLine = new StatusLine(statusString);

        //check for a valid HTTP-Version
        String httpVersion = statusLine.getHttpVersion();
        if (httpVersion.equals("HTTP/1.0")){
            http11 = false;
        } else if (httpVersion.equals("HTTP/1.1")){
            http11 = true;
        } else {
            throw new HttpException("Unrecognized server protocol: '"
                                    + httpVersion + "'");
        }

    }

    // ------------------------------------------------------ Protected Methods

    /**
     * <p>
     * Writes my request to the given {@link HttpConnection}.
     * </p>
     *
     * <p>
     * The request is written according to the following logic:
     * </p>
     *
     * <ol>
     * <li>
     * {@link #writeRequestLine writeRequestLine(HttpState, HttpConnection)} is
     * invoked to write the request line.
     * </li>
     * <li>
     * {@link #writeRequestHeaders writeRequestHeaders(HttpState,
     * HttpConnection)} is invoked to write the associated headers.
     * </li>
     * <li>
     * <tt>\r\n</tt> is sent to close the head part of the request.
     * </li>
     * <li>
     * {@link #writeRequestBody writeRequestBody(HttpState, HttpConnection)} is
     * invoked to write the body part of the request.
     * </li>
     * </ol>
     *
     * <p>
     * Subclasses may want to override one or more of the above methods to to
     * customize the processing. (Or they may choose to override this method
     * if dramatically different processing is required.)
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to write the request to
     * @throws IOException when i/o errors occur reading the response
     * @throws HttpException when a protocol error occurs or state is invalid
     */
    protected void writeRequest(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace(
            "enter HttpMethodBase.writeRequest(HttpState, HttpConnection)");
        writeRequestLine(state, conn);
        writeRequestHeaders(state, conn);
        conn.writeLine(); // close head
        bodySent = writeRequestBody(state, conn);
    }

    /**
     * Write the request body to the given {@link HttpConnection}.
     *
     * <p>
     * If an expectation is required, this method should ensure that it has
     * been sent by checking the {@link #getStatusCode status code}.
     * </p>
     *
     * <p>
     * This method should return <tt>true</tt> if the request body was actually
     * sent (or is empty), or <tt>false</tt> if it could not be sent for some
     * reason (for example, expectation required but not present).
     * </p>
     *
     * <p>
     * This implementation writes nothing and returns <tt>true</tt>.
     * </p>
     *
     * @param state the client state
     * @param conn the connection to write to
     *
     * @return <tt>true</tt>
     * @throws IOException when i/o errors occur reading the response
     * @throws HttpException when a protocol error occurs or state is invalid
     */
    protected boolean writeRequestBody(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        return true;
    }

    /**
     * Writes the request headers to the given {@link HttpConnection}.
     *
     * <p>
     * This implementation invokes {@link #addRequestHeaders
     * addRequestHeaders(HttpState,HttpConnection)}, and then writes each
     * header to the request stream.
     * </p>
     *
     * <p>
     * Subclasses may want to override this method to to customize the
     * processing.
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to write to
     * @throws IOException when i/o errors occur reading the response
     * @throws HttpException when a protocol error occurs or state is invalid
     *
     * @see #addRequestHeaders
     * @see #getRequestHeaders
     */
    protected void writeRequestHeaders(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter HttpMethodBase.writeRequestHeaders(HttpState,"
            + "HttpConnection)");
        addRequestHeaders(state, conn);
        Iterator it = requestHeaders.values().iterator();
        while (it.hasNext()) {
            conn.print(((Header) it.next()).toExternalForm());
        }
    }

    /**
     * Writes the "request line" to the given {@link HttpConnection}.
     *
     * <p>
     * Subclasses may want to override this method to to customize the
     * processing.
     * </p>
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} to write to
     * @throws IOException when i/o errors occur reading the response
     * @throws HttpException when a protocol error occurs or state is invalid
     *
     * @see #generateRequestLine
     */
    protected void writeRequestLine(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace(
            "enter HttpMethodBase.writeRequestLine(HttpState, HttpConnection)");
        String requestLine = getRequestLine(conn);
        conn.print(requestLine);
    }

    /**
     * Gets the request line that was sent to the http server.
     * Consider making this public.  Consider creating a new class
     * RequestLine for this purpose.
     */
    private String getRequestLine(HttpConnection conn) {
        return  HttpMethodBase.generateRequestLine(conn, getName(),
                getPath(), getQueryString(), getHttpVersion());
    }

    /**
     * Get the HTTP version.
     *
     * @return HTTP/1.1 if http11, HTTP/1.0 otherwise
     *
     * @since 2.0
     */
    private String getHttpVersion() {
        return (http11 ? "HTTP/1.1" : "HTTP/1.0");
    }

    /**
     * Determines if the provided value is a valid IPv4 internet address.
     *
     * @param value - value to check
     *
     * @return boolean - true if value is valid, otherwise false
     */
    private static boolean isIpAddress(String value) {
        log.trace("enter HttpMethodBase.isIpAddress(String)");

        value = value.trim();

        // prevent input values of 127.0.0.1. or .127.0.0.1, etc.
        if (value.startsWith(".") || value.endsWith(".")) {
            return false;
        }

        StringTokenizer tokenizer = new StringTokenizer(value, ".");
        if (tokenizer.countTokens() == 4) {
            while (tokenizer.hasMoreTokens()) {
                try {
                    int i = Integer.parseInt(tokenizer.nextToken());
                    if ((i < 0) || (i > 255)) {
                        // parsed section of address is not in the proper range
                        return false;
                    }
                } catch (NumberFormatException nfe) {
                    return false;
                }
            }
        } else {
            // wrong number of tokens
            return false;
        }
        return true;
    }


    /**
     * "It must be possible to combine the multiple header fields into one
     * "field-name: field-value" pair, without changing the semantics of the
     * message, by appending each subsequent field-value to the first, each
     * separated by a comma."
     * //TODO: This method is trying to make up for deficiencies in Header.
     *
     * @param existingHeader the current header
     * @param value DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    private String getNewHeaderValue(Header existingHeader, String value) {
        String existingValue = existingHeader.getValue();
        if (existingValue == null) {
            existingValue = "";
        }
        String newValue = value;
        if (value == null) {
            newValue = "";
        }
        return existingValue + ", " + newValue;
    }

    /**
     * Sets the specified response header.
     *
     * @param header the header to set.
     *
     * @since 2.0
     */
    private void setResponseHeader(Header header) {
        if (header == null) {
            return;
        }
        responseHeaders.put(header.getName().toLowerCase(), header);
    }

    /**
     * Per RFC 2616 section 4.3, some response can never contain a message
     * body.
     *
     * @param status - the HTTP status code
     *
     * @return true if the message may contain a body, false if it can not
     *         contain a message body
     */
    private static boolean canResponseHaveBody(int status) {
        log.trace("enter HttpMethodBase.canResponseHaveBody(int)");

        boolean result = true;

        if ((status >= 100 && status <= 199) || (status == 204)
            || (status == 304)) { // NOT MODIFIED
            result = false;
        }

        return result;
    }

    /**
     * process a response that requires authentication
     *
     * @param state the current state
     *
     * @return true if the request has completed process, false if more
     *         attempts are needed
     */
    private boolean processAuthenticationResponse(HttpState state) {
        log.trace("enter HttpMethodBase.processAuthenticationResponse("
            + "HttpState, HttpConnection)");

        int statusCode = statusLine.getStatusCode();
        // handle authentication required
        Header wwwauth = null;
        Set realmsUsed = null;
        switch (statusCode) {
            case HttpStatus.SC_UNAUTHORIZED:
                wwwauth = getResponseHeader(Authenticator.WWW_AUTH);
                realmsUsed = realms;
                break;
            case HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED:
                wwwauth = getResponseHeader(Authenticator.PROXY_AUTH);
                realmsUsed = proxyRealms;
                break;
        }
        boolean authenticated = false;
        // if there was a header requesting authentication
        if (null != wwwauth) {
            String pathAndCreds = getPath() + ":" + wwwauth.getValue();
            if (realmsUsed.contains(pathAndCreds)) {
                if (log.isInfoEnabled()) {
                    log.info("Already tried to authenticate to \""
                             + wwwauth.getValue() + "\" but still receiving "
                             + statusCode + ".");
                }
                return true;
            } else {
                realmsUsed.add(pathAndCreds);
            }

            try {
                //remove preemptive header and reauthenticate
                switch (statusCode) {
                    case HttpStatus.SC_UNAUTHORIZED:
                        removeRequestHeader(Authenticator.WWW_AUTH_RESP);
                        authenticated = Authenticator.authenticate(this, state);
                        break;
                    case HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED:
                        removeRequestHeader(Authenticator.PROXY_AUTH_RESP);
                        authenticated = Authenticator.authenticateProxy(this,
                                                                        state);
                        break;
                }
            } catch (HttpException httpe) {
                log.warn(httpe.getMessage());
                return true; // finished request
            } catch (UnsupportedOperationException uoe) {
                log.warn(uoe.getMessage());
                //FIXME: should this return true?
            }

            if (!authenticated) {
                // won't be able to authenticate to this challenge
                // without additional information
                log.debug("HttpMethodBase.execute(): Server demands "
                          + "authentication credentials, but none are "
                          + "available, so aborting.");
            } else {
                log.debug("HttpMethodBase.execute(): Server demanded "
                          + "authentication credentials, will try again.");
                // let's try it again, using the credentials
            }
        }

        return !authenticated; // finished processing if we aren't authenticated
    }

    /**
     * Write a request and read the response. Both the write to the server will
     * be retried {@link #maxRetries} times if the operation fails with a
     * HttpRecoverableException. The write will only be attempted if the read
     * has succeeded.
     *
     * <p>
     * The <i>used</i> is set to true if the write succeeds.
     * </p>
     *
     * @param state the current state
     * @param connection the connection for communication
     *
     * @throws HttpException when errors occur as part of the HTTP protocol
     *         conversation
     * @throws IOException when an I/O error occurs communicating with the
     *         server
     *
     * @see #writeRequest(HttpState,HttpConnection)
     * @see #readResponse(HttpState,HttpConnection)
     */
    private void processRequest(HttpState state, HttpConnection connection)
    throws HttpException, IOException {
        log.trace(
            "enter HttpMethodBase.processRequest(HttpState, HttpConnection)");

        //try to do the write
        int retryCount = 0;
        do {
            retryCount++;
            if (log.isTraceEnabled()) {
                log.trace("Attempt number " + retryCount + " to write request");
            }
            try {
                if (!connection.isOpen()) {
                    log.debug("Opening the connection.");
                    connection.open();
                }
                writeRequest(state, connection);
                used = true; //write worked, mark this method as used
                break; //move onto the write
            } catch (HttpRecoverableException httpre) {
                if (log.isDebugEnabled()) {
                    log.debug("Closing the connection.");
                }

                // update the recoverable exception count.
                recoverableExceptionCount++;

                connection.close();
                log.info("Recoverable exception caught when writing request");
                if (retryCount == maxRetries) {
                    log.warn(
                        "Attempt to write request has reached max retries: "
                        + maxRetries);
                    throw httpre;
                }
            }
        } while (retryCount <= maxRetries);

        //try to do the read
        try {
            readResponse(state, connection);
        } catch (HttpRecoverableException httpre) {
            log.warn("Recoverable exception caught when reading response");
            if (log.isDebugEnabled()) {
                log.debug("Closing the connection.");
            }

            connection.close();
            throw httpre;
        }
        //everything should be OK at this point
    }

    /**
     * On a {@link HttpStatus#SC_CONTINUE continue}, if there are more request
     * bytes to be sent, write them to the connection
     *
     * @param state the current state
     * @param connection the connection for communication
     *
     * @throws HttpException when errors occur as part of the HTTP protocol
     *         conversation
     * @throws IOException when an I/O error occurs communicating with the
     *         server
     */
    private void writeRemainingRequestBody(HttpState state,
                                           HttpConnection connection)
    throws HttpException, IOException {
        log.trace("enter writeRemainingRequestBody(HttpState, HttpConnection)");

        if (HttpStatus.SC_CONTINUE == statusLine.getStatusCode()) {
            if (!bodySent) {
                bodySent = writeRequestBody(state, connection);
            } else {
                log.warn("Received status CONTINUE but the body has already "
                    + "been sent");
                // According to RFC 2616 this respose should be ignored
            }
            readResponse(state, connection);
        }
    }


    protected static String getContentCharSet(Header contentheader)
    {
        log.trace("enter getContentCharSet( Header contentheader )");
        String charset = null;
        if (contentheader != null){
            try {
                HeaderElement values[] = contentheader.getValues();
                // I expect only one header element to be there
                // No more. no less
                if (values.length == 1) {
                    NameValuePair param = values[0].getParameterByName("charset");
                    if (param != null) {
                        // If I get anything "funny" UnsupportedEncondingException will result
                        charset = param.getValue();
                    }
                }
            }
            catch(HttpException e){
                log.error(e);
            }
        }
        if (charset == null) {
            if (log.isDebugEnabled()) {
                log.debug("Default charset used: " + HttpConstants.DEFAULT_CONTENT_CHARSET);
            }
            charset = HttpConstants.DEFAULT_CONTENT_CHARSET;
        }
        return charset;
    }


    public String getRequestCharSet() {
        return getContentCharSet(getRequestHeader("Content-Type"));
    }


    public String getResponseCharSet() {
        return getContentCharSet(getResponseHeader("Content-Type"));
    }

    /**
     * Returns the number of "recoverable" exceptions thrown and handled, to
     * allow for monitoring the quality of the connection.
     *
     * @return The number of recoverable exceptions handled by the method.
     */
    public int getRecoverableExceptionCount() {
        return recoverableExceptionCount;
    }

    /**
     * A response has been consumed.
     *
     * <p>The default behavior for this class is to check to see if the connection
     * should be closed, and close if need be, and to ensure that the connection
     * is returned to the connection manager - if and only if we are not still
     * inside the execute call.</p>
     *
     */
    protected void responseBodyConsumed() {

        // make sure this is the initial invocation of the notification,
        // ignore subsequent ones.
        responseStream = null;
        responseConnection.setLastResponseInputStream(null);

        if (shouldCloseConnection()) {
            responseConnection.close();
        }

        doneWithConnection = true;
        if (!inExecute) {
            ensureConnectionRelease();
        }
    }

    /**
     * Insure that the connection is released back to the pool.
     */
    private void ensureConnectionRelease() {
        if ( responseConnection != null ) {
            responseConnection.releaseConnection();
            responseConnection = null;
        }
    }

    /**
     * This exists so that the public interface to this class need not include
     * either the responseConsumed or the responseBodyConsumed methods.
     */
    private ResponseConsumedWatcher m_responseWatcher = new ResponseConsumedWatcher() {
        public void responseConsumed() {
            responseBodyConsumed();
        }
    };

    /**
     * Returns the hostConfiguration.
     * @return HostConfiguration
     */
    public HostConfiguration getHostConfiguration() {
        return hostConfiguration;
    }

    /**
     * Sets the hostConfiguration.
     * @param hostConfiguration The hostConfiguration to set
     */
    public void setHostConfiguration(HostConfiguration hostConfiguration) {
        this.hostConfiguration = hostConfiguration;
    }

}
