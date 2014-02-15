/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpMethodBase.java,v 1.159.2.5 2003/07/26 14:07:34 olegk Exp $
 * $Revision: 1.159.2.5 $
 * $Date: 2003-08-01 03:46:00 +0200 (Fri, 01 Aug 2003) $
 *
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
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
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
import java.io.InterruptedIOException;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.httpclient.auth.AuthScheme;
import org.apache.commons.httpclient.auth.AuthenticationException;
import org.apache.commons.httpclient.auth.HttpAuthenticator;
import org.apache.commons.httpclient.auth.MalformedChallengeException;
import org.apache.commons.httpclient.cookie.CookiePolicy;
import org.apache.commons.httpclient.cookie.CookieSpec;
import org.apache.commons.httpclient.cookie.MalformedCookieException;
import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.httpclient.util.EncodingUtil;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * An abstract base implementation of HttpMethod.
 * <p>
 * At minimum, subclasses will need to override:
 * <ul>
 *   <li>{@link #getName} to return the approriate name for this method
 *   </li>
 * </ul>
 *
 * <p>
 * When a method's request may contain a body, subclasses will typically want
 * to override:
 * <ul>
 *   <li>{@link #getRequestContentLength} to indicate the length (in bytes)
 *     of that body</li>
 *   <li>{@link #writeRequestBody writeRequestBody(HttpState,HttpConnection)}
 *     to write the body</li>
 * </ul>
 * </p>
 *
 * <p>
 * When a method requires additional request headers, subclasses will typically
 * want to override:
 * <ul>
 *   <li>{@link #addRequestHeaders addRequestHeaders(HttpState,HttpConnection)}
 *      to write those headers
 *   </li>
 * </ul>
 * </p>
 *
 * <p>
 * When a method expects specific response headers, subclasses may want to
 * override:
 * <ul>
 *   <li>{@link #processResponseHeaders processResponseHeaders(HttpState,HttpConnection)}
 *     to handle those headers
 *   </li>
 * </ul>
 * </p>
 *
 *
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
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 *
 * @version $Revision: 1.159.2.5 $ $Date: 2003-08-01 03:46:00 +0200 (Fri, 01 Aug 2003) $
 */
public abstract class HttpMethodBase implements HttpMethod {

    /** Maximum number of redirects and authentications that will be followed */
    private static final int MAX_FORWARDS = 100;

    // -------------------------------------------------------------- Constants

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(HttpMethodBase.class);

    /** The User-Agent header sent on every request. */
    protected static final Header USER_AGENT;

    static {
        String agent = System.getProperties()
                             .getProperty("httpclient.useragent",
                                          "Jakarta Commons-HttpClient/2.0rc1");
        USER_AGENT = new Header("User-Agent", agent);
    }

    // ----------------------------------------------------- Instance variables 

    /** My request headers, if any. */
    private HeaderGroup requestHeaders = new HeaderGroup();

    /** The Status-Line from the response. */
    private StatusLine statusLine = null;

    /** My response headers, if any. */
    private HeaderGroup responseHeaders = new HeaderGroup();

    /** My response trailer headers, if any. */
    private HeaderGroup responseTrailerHeaders = new HeaderGroup();

    /** Realms that we tried to authenticate to */
    private Set realms = null;

    /** Actual authentication realm */
    private String realm = null;

    /** Proxy Realms that we tried to authenticate to */
    private Set proxyRealms = null;

    /** Actual proxy authentication realm */
    private String proxyRealm = null;

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

    /** Whether or not I should automatically follow redirects. */
    private boolean followRedirects = false;

    /** Whether or not I should automatically process authentication. */
    private boolean doAuthentication = true;

    /** Whether or not I should use the HTTP/1.1 protocol. The default is <tt>true</tt>. */
    private boolean http11 = true;

    /** True if we're in strict mode. The default is <tt>false</tt>. */
    private boolean strictMode = false;

    /** Whether or not I have been executed. */
    private boolean used = false;

    /** How many times did this transparently handle a recoverable exception? */
    private int recoverableExceptionCount = 0;

    /** the host configuration for this method, can be null */
    private HostConfiguration hostConfiguration;

    /**
     * Handles method retries
     */
    private MethodRetryHandler methodRetryHandler;

    /** true if we are currently executing */
    private boolean inExecute = false;

    /** true if we are finished with the connection */
    private boolean doneWithConnection = false;

    /** true if the connection must be closed when no longer needed */
    private boolean connectionCloseForced = false;

    /** Number of milliseconds to wait for 100-contunue response. */
    private static final int RESPONSE_WAIT_TIME_MS = 3000;

    // ----------------------------------------------------------- Constructors

    /**
     * No-arg constructor.
     */
    public HttpMethodBase() {
    }

    /**
     * Constructor specifying a URI.
     * It is responsibility of the caller to ensure that URI elements
     * (path & query parameters) are properly encoded (URL safe).
     *
     * @param uri either an absolute or relative URI. The URI is expected
     *            to be URL-encoded
     * 
     * @throws IllegalArgumentException when URI is invalid
     * @throws IllegalStateException when protocol of the absolute URI is not recognised
     */
    public HttpMethodBase(String uri) 
        throws IllegalArgumentException, IllegalStateException {

        try {

            // create a URI and allow for null/empty uri values
            if (uri == null || uri.equals("")) {
                uri = "/";
            }
            URI parsedURI = new URI(uri.toCharArray());
            
            // only set the host if specified by the URI
            if (parsedURI.isAbsoluteURI()) {
                hostConfiguration = new HostConfiguration();
                hostConfiguration.setHost(
                    parsedURI.getHost(),
                    parsedURI.getPort(),
                    parsedURI.getScheme()
                ); 
            }
            
            // set the path, defaulting to root
            setPath(
                parsedURI.getPath() == null
                ? "/"
                : parsedURI.getEscapedPath()
            );
            setQueryString(parsedURI.getEscapedQuery());

        } catch (URIException e) {
            throw new IllegalArgumentException("Invalid uri '" 
                + uri + "': " + e.getMessage() 
            );
        }
    }

    // ------------------------------------------- Property Setters and Getters

    /**
     * Obtain the name of this method, suitable for use in the "request line",
     * for example <tt>GET</tt> or <tt>POST</tt>.
     *
     * @return the name of this method
     */
    public abstract String getName();

    /**
     * Return the URI
     * @return The URI
     * @throws URIException If the URI cannot be created.
     * @see org.apache.commons.httpclient.HttpMethod#getURI()
     */
    public URI getURI() throws URIException {

        if (hostConfiguration == null) {
            // just use a relative URI, the host hasn't been set
            URI tmpUri = new URI(null, null, path, null, null);
            tmpUri.setEscapedQuery(queryString);
            return tmpUri;
        } else {

            // we only want to include the port if it's not the default
            int port = hostConfiguration.getPort();
            if (port == hostConfiguration.getProtocol().getDefaultPort()) {
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
     * HTTP/1.1 protocol. The default is <tt>true</tt>. 
     *
     * @return <tt>true</tt> if I should use the HTTP/1.1 protocol
     */
    public boolean isHttp11() {
        return http11;
    }

    /**
     * Set the path part of my request.
     * It is responsibility of the caller to ensure that the path is
     * properly encoded (URL safe).
     *
     * @param path the path to request. The path is expected
     *        to be URL-encoded
     */
    public void setPath(String path) {
        this.path = path;
    }

    /**
     * Add the specified request header. A <i>header</i> value of
     * <code>null</code> will be ignored. Note that header-name matching is case
     * insensitive.
     *
     * @param header the header to add to the request
     */
    public void addRequestHeader(Header header) {
        LOG.trace("HttpMethodBase.addRequestHeader(Header)");

        if (header == null) {
            LOG.debug("null header value ignored");
        } else {
            getRequestHeaderGroup().addHeader(header);
        }
    }

    /**
     * adds a response footer to the internal list
     * @param footer The new footer to add.
     */
    public void addResponseFooter(Header footer) {
        getResponseTrailerHeaderGroup().addHeader(footer);
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
     * Sets the query string. The user must ensure that the string is properly 
     * URL encoded. The query string should not start with the question mark character.
     *
     * @param queryString the query string
     * 
     * @see EncodingUtil#formUrlEncode(NameValuePair[], String)
     */
    public void setQueryString(String queryString) {
        this.queryString = queryString;
    }

    /**
     * Sets the query string.  The pairs are encoded as UTF-8 characters.  To use
     * a different charset the parameters can be encoded manually using EncodingUtil 
     * and set as a single String.
     *
     * @param params an array of {@link NameValuePair}s to add as query string
     *        parameters. The name/value pairs will be automcatically 
     *        URL encoded
     * 
     * @see EncodingUtil#formUrlEncode(NameValuePair[], String)
     * @see #setQueryString(String)
     */
    public void setQueryString(NameValuePair[] params) {
        LOG.trace("enter HttpMethodBase.setQueryString(NameValuePair[])");
        queryString = EncodingUtil.formUrlEncode(params, "UTF-8");
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
        
        Header[] headers = getRequestHeaderGroup().getHeaders(header.getName());
        
        for (int i = 0; i < headers.length; i++) {
            getRequestHeaderGroup().removeHeader(headers[i]);
        }
        
        getRequestHeaderGroup().addHeader(header);
        
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
        if (headerName == null) {
            return null;
        } else {
            return getRequestHeaderGroup().getCondensedHeader(headerName);
        }
    }

    /**
     * Provides access to the request headers.
     *
     * @return an array of my request headers.
     */
    public Header[] getRequestHeaders() {
        return getRequestHeaderGroup().getAllHeaders();
    }

    /**
     * Gets the HeaderGroup storing the request headers.
     * 
     * @return a HeaderGroup
     * 
     * @since 2.0beta1
     */
    protected HeaderGroup getRequestHeaderGroup() {
        return requestHeaders;
    }

    /**
     * Gets the HeaderGroup storing the response trailer headers as per RFC
     * 2616 section 3.6.1.
     * 
     * @return a HeaderGroup
     * 
     * @since 2.0beta1
     */
    protected HeaderGroup getResponseTrailerHeaderGroup() {
        return responseTrailerHeaders;
    }

    /**
     * Gets the HeaderGroup storing the response headers.
     * 
     * @return a HeaderGroup
     * 
     * @since 2.0beta1
     */
    protected HeaderGroup getResponseHeaderGroup() {
        return responseHeaders;
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
     * Gets the response headers in the order in which they were read.
     *
     * @return an array of my response headers.
     */
    public Header[] getResponseHeaders() {
        return getResponseHeaderGroup().getAllHeaders();
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
        if (headerName == null) {
            return null;
        } else {
            return getResponseHeaderGroup().getCondensedHeader(headerName);
        }        
    }


    /**
     * Return the length (in bytes) of my response body, as specified in a
     * <tt>Content-Length</tt> header.
     *
     * <p>
     * Return <tt>-1</tt> when the content-length is unknown.
     * </p>
     *
     * @return content length, if <tt>Content-Length</tt> header is available. 
     *          <tt>0</tt> indicates that the request has no body.
     *          If <tt>Content-Length</tt> header is not present, the method 
     *          returns  <tt>-1</tt>.
     */
    protected int getResponseContentLength() {
        Header[] headers = getResponseHeaderGroup().getHeaders("Content-Length");
        if (headers.length == 0) {
            return -1;
        }
        if (headers.length > 1) {
            LOG.warn("Multiple content-length headers detected");
        }
        for (int i = headers.length - 1; i >= 0; i++) {
            Header header = headers[i];
            try {
                return Integer.parseInt(header.getValue());
            } catch (NumberFormatException e) {
                if (LOG.isWarnEnabled()) {
                    LOG.warn("Invalid content-length value: " + e.getMessage());
                }
            }
            // See if we can have better luck with another header, if present
        }
        return -1;
    }


    /**
     * Return my response body, if any, as a byte array. Otherwise return
     * <tt>null</tt>.
     * @return The response body as a byte array.
     */
    public byte[] getResponseBody() {
        if (this.responseBody == null) {
            try {
                InputStream instream = getResponseBodyAsStream();
                if (instream != null) {
                    LOG.debug("Buffering response body");
                    ByteArrayOutputStream outstream = new ByteArrayOutputStream();
                    byte[] buffer = new byte[4096];
                    int len;
                    while ((len = instream.read(buffer)) > 0) {
                        outstream.write(buffer, 0, len);
                    }
                    outstream.close();
                    setResponseStream(null);
                    this.responseBody = outstream.toByteArray();
                }
            } catch (IOException e) {
                LOG.error("I/O failure reading response body", e);
                this.responseBody = null;
            }
        }
        return this.responseBody;
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
            LOG.debug("re-creating response stream from byte array");
            return byteResponseStream;
        }
        return null;
    }

    /**
     * Gets the response body as a string.
     *
     * <b>Note:</b> The string conversion done on the data is done with the
     * default character encoding.  The use of this method may be non-portable.
     * To ensure portability, you can use {@link #getResponseBody()} to get
     * the body as an array of bytes and then do your own character encoding.
     *
     * @return Stringified form of the responseBody if it exists, 
     *   otherwise <tt>null</tt>.
     */
    public String getResponseBodyAsString() {
        byte[] rawdata = null;
        if (responseAvailable()) {
            rawdata = getResponseBody();
        }
        if (rawdata != null) {
            return HttpConstants.getContentString(rawdata, getResponseCharSet());
        } else {
            return null;
        }
    }

    /**
     * Gets the response footers in the order in which they were read.
     * @return an array of headers
     */
    public Header[] getResponseFooters() {
        return getResponseTrailerHeaderGroup().getAllHeaders();
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
        if (footerName == null) {
            return null;
        } else {
            return getResponseTrailerHeaderGroup().getCondensedHeader(footerName);
        }
    }

    /**
     * Set the response stream.
     * @param responseStream The new response stream.
     */
    protected void setResponseStream(InputStream responseStream) {
        this.responseStream = responseStream;
    }

    /**
     * @return the current response stream
     */
    protected InputStream getResponseStream() {
        return responseStream;
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
     * Defines how strictly HttpClient follows the HTTP protocol specification  
     * (RFC 2616 and other relevant RFCs). In the strict mode HttpClient precisely
     * implements the requirements of the specification, whereas in non-strict mode 
     * it attempts to mimic the exact behaviour of commonly used HTTP agents, 
     * which many HTTP servers expect.
     * 
     * @param strictMode true for strict mode, false otherwise
     */
    public void setStrictMode(boolean strictMode) {
        this.strictMode = strictMode;
    }

    /**
     * Returns the value of strictMode.
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
        addRequestHeader(new Header(headerName, headerValue));
    }

    /**
     * Tests if the connection should be force-closed when no longer needed.
     * 
     * @return <code>true</code> if the connection must be closed
     */
    protected boolean isConnectionCloseForced() {
        return this.connectionCloseForced;
    }

    /**
     * Sets whether or not the connection should be force-closed when no longer 
     * needed. This value should only be set to <code>true</code> in abnormal 
     * circumstances. 
     * 
     * @param b <code>true</code> if the connection must be closed, <code>false</code>
     * otherwise.
     */
    protected void setConnectionCloseForced(boolean b) {
        this.connectionCloseForced = b;
    }

    /**
     * Return true if we should close the connection now.  The connection will
     * only be left open if we are using HTTP1.1 or if "Connection: keep-alive" 
     * was sent.
     * 
     * @param conn the connection in question
     * 
     * @return boolean true if we should close the connection.
     */
    protected boolean shouldCloseConnection(HttpConnection conn) {

        // Connection must be closed due to an abnormal circumstance 
        if (isConnectionCloseForced()) {
            LOG.debug("Should forcefully close connection.");
            return true;
        }

        Header connectionHeader = null;
        // In case being connected via a proxy server
        if (!conn.isTransparent()) {
            // Check for 'proxy-connection' directive
            connectionHeader = responseHeaders.getFirstHeader("proxy-connection");
        }
        // In all cases Check for 'connection' directive
        // some non-complaint proxy servers send it instread of
        // expected 'proxy-connection' directive
        if (connectionHeader == null) {
            connectionHeader = responseHeaders.getFirstHeader("connection");
        }
        if (connectionHeader != null) {
            if (connectionHeader.getValue().equalsIgnoreCase("close")) {
                if (LOG.isDebugEnabled()) {
                    LOG.debug("Should close connection in response to " 
                        + connectionHeader.toExternalForm());
                }
                return true;
            } else if (connectionHeader.getValue().equalsIgnoreCase("keep-alive")) {
                if (LOG.isDebugEnabled()) {
                    LOG.debug("Should NOT close connection in response to " 
                        + connectionHeader.toExternalForm());
                }
                return false;
            } else {
                if (LOG.isDebugEnabled()) {
                    LOG.debug("Unknown directive: " + connectionHeader.toExternalForm());
                }
            }
        }
        LOG.debug("Resorting to protocol version default close connection policy");
        // missing or invalid connection header, do the default
        if (http11) {
            LOG.debug("Should NOT close connection, using HTTP/1.1.");
        } else {
            LOG.debug("Should close connection, using HTTP/1.0.");
        }
        return !http11;
    }
    
    /**
     * Return true if a retry is needed.
     * @param statusCode The status code
     * @param state The state.
     * @param conn The connection
     * @return boolean true if a retry is needed.
     */
    private boolean isRetryNeeded(int statusCode, HttpState state, HttpConnection conn) {
        switch (statusCode) {
            case HttpStatus.SC_UNAUTHORIZED:
            case HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED:
                LOG.debug("Authorization required");
                if (doAuthentication) { //process authentication response
                    //if the authentication is successful, return the statusCode
                    //otherwise, drop through the switch and try again.
                    if (processAuthenticationResponse(state, conn)) {
                        return false;
                    }
                } else { //let the client handle the authenticaiton
                    return false;
                }
                break;

            case HttpStatus.SC_MOVED_TEMPORARILY:
            case HttpStatus.SC_MOVED_PERMANENTLY:
            case HttpStatus.SC_SEE_OTHER:
            case HttpStatus.SC_TEMPORARY_REDIRECT:
                LOG.debug("Redirect required");

                if (!processRedirectResponse(conn)) {
                    return false;
                }
                break;

            default:
                // neither an unauthorized nor a redirect response
                return false;
        } //end of switch

        return true;
    }

    /**
     * Check to see if the this method is ready to be executed.
     * 
     * @param state The state.
     * @param conn The connection.
     * @throws HttpException If the method isn't valid.
     */
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
        throws HttpException, HttpRecoverableException, 
            IOException, NullPointerException {
                
        LOG.trace("enter HttpMethodBase.execute(HttpState, HttpConnection)");

        // this is our connection now, assign it to a local variable so 
        // that it can be released later
        this.responseConnection = conn;

        checkExecuteConditions(state, conn);
        inExecute = true;

        try {
            //pre-emptively add the authorization header, if required.
            if (state.isAuthenticationPreemptive()) {

                LOG.debug("Preemptively sending default basic credentials");

                try {
                    if (HttpAuthenticator.authenticateDefault(this, conn, state)) {
                        LOG.debug("Default basic credentials applied");
                    }
                    if (conn.isProxied()) {
                        if (HttpAuthenticator.authenticateProxyDefault(this, conn, state)) {
                            LOG.debug("Default basic proxy credentials applied");
                        }
                    }
                } catch (AuthenticationException e) {
                    // Log error and move on
                    LOG.error(e.getMessage(), e);
                }
            }

            realms = new HashSet();
            proxyRealms = new HashSet();
            int forwardCount = 0; //protect from an infinite loop

            while (forwardCount++ < MAX_FORWARDS) {
                // on every retry, reset this state information.
                conn.setLastResponseInputStream(null);

                if (LOG.isDebugEnabled()) {
                    LOG.debug("Execute loop try " + forwardCount);
                }

                // Discard status line
                this.statusLine = null;

                //write the request and read the response, will retry
                processRequest(state, conn);

                if (!isRetryNeeded(statusLine.getStatusCode(), state, conn)) {
                    // nope, no retry needed, exit loop.
                    break;
                }

                // retry - close previous stream.  Caution - this causes
                // responseBodyConsumed to be called, which may also close the
                // connection.
                if (responseStream != null) {
                    responseStream.close();
                }

            } //end of retry loop

            if (forwardCount >= MAX_FORWARDS) {
                LOG.error("Narrowly avoided an infinite loop in execute");
                throw new HttpRecoverableException("Maximum redirects ("
                    + MAX_FORWARDS + ") exceeded");
            }

        } finally {
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

    /**
     * Process the redirect response.
     * @param conn The connection to use.
     * @return boolean true if the redirect was successful.
     */
    private boolean processRedirectResponse(HttpConnection conn) {

        if (!getFollowRedirects()) {
            LOG.info("Redirect requested but followRedirects is "
                    + "disabled");
            return false;
        }

        //get the location header to find out where to redirect to
        Header locationHeader = getResponseHeader("location");
        if (locationHeader == null) {
            // got a redirect response, but no location header
            LOG.error("Received redirect response " + getStatusCode()
                    + " but no location header");
            return false;
        }
        String location = locationHeader.getValue();
        if (LOG.isDebugEnabled()) {
            LOG.debug("Redirect requested to location '" + location
                    + "'");
        }

        //rfc2616 demands the location value be a complete URI
        //Location       = "Location" ":" absoluteURI
        URI redirectUri = null;
        URI currentUri = null;

        try {
            currentUri = new URI(
                conn.getProtocol().getScheme(),
                null,
                conn.getHost(), 
                conn.getPort(), 
                this.getPath()
            );
            redirectUri = new URI(location.toCharArray());
            if (redirectUri.isRelativeURI()) {
                if (isStrictMode()) {
                    LOG.warn("Redirected location '" + location 
                        + "' is not acceptable in strict mode");
                    return false;
                } else { 
                    //location is incomplete, use current values for defaults
                    LOG.debug("Redirect URI is not absolute - parsing as relative");
                    redirectUri = new URI(currentUri, redirectUri);
                }
            }
        } catch (URIException e) {
            LOG.warn("Redirected location '" + location + "' is malformed");
            return false;
        }

        //check for redirect to a different protocol, host or port
        try {
            checkValidRedirect(currentUri, redirectUri);
        } catch (HttpException ex) {
            //LOG the error and let the client handle the redirect
            LOG.warn(ex.getMessage());
            return false;
        }

        //invalidate the list of authentication attempts
        this.realms.clear();
        //remove exisitng authentication headers
        removeRequestHeader(HttpAuthenticator.WWW_AUTH_RESP); 
        //update the current location with the redirect location.
        //avoiding use of URL.getPath() and URL.getQuery() to keep
        //jdk1.2 comliance.
        setPath(redirectUri.getEscapedPath());
        setQueryString(redirectUri.getEscapedQuery());

        if (LOG.isDebugEnabled()) {
            LOG.debug("Redirecting from '" + currentUri.getEscapedURI()
                + "' to '" + redirectUri.getEscapedURI());
        }

        return true;
    }

    /**
     * Check for a valid redirect given the current conn and new URI.
     * Redirect to a different protocol, host or port are checked for validity.
     *
     * @param currentUri The current URI (redirecting from)
     * @param redirectUri The new URI to redirect to
     * @throws HttpException if the redirect is invalid
     * @since 2.0
     */
    private static void checkValidRedirect(URI currentUri, URI redirectUri)
    throws HttpException {
        LOG.trace("enter HttpMethodBase.checkValidRedirect(HttpConnection, URL)");

        String oldProtocol = currentUri.getScheme();
        String newProtocol = redirectUri.getScheme();
        if (!oldProtocol.equals(newProtocol)) {
            throw new HttpException("Redirect from protocol " + oldProtocol
                    + " to " + newProtocol + " is not supported");
        }

        try {
            String oldHost = currentUri.getHost();
            String newHost = redirectUri.getHost();
            if (!oldHost.equalsIgnoreCase(newHost)) {
                throw new HttpException("Redirect from host " + oldHost
                        + " to " + newHost + " is not supported");
            }
        } catch (URIException e) {
            LOG.warn("Error getting URI host", e);
            throw new HttpException("Invalid Redirect URI from: " 
                + currentUri.getEscapedURI() + " to: " + redirectUri.getEscapedURI()
            );
        }

        int oldPort = currentUri.getPort();
        if (oldPort < 0) {
            oldPort = getDefaultPort(oldProtocol);
        }
        int newPort = redirectUri.getPort();
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
        if (proto.equals("http")) {
            return 80;
        } else if (proto.equals("https")) {
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
        LOG.trace("enter HttpMethodBase.recycle()");

        releaseConnection();

        path = null;
        followRedirects = false;
        doAuthentication = true;
        realm = null;
        proxyRealm = null;
        queryString = null;
        getRequestHeaderGroup().clear();
        getResponseHeaderGroup().clear();
        getResponseTrailerHeaderGroup().clear();
        statusLine = null;
        used = false;
        http11 = true;
        responseBody = null;
        recoverableExceptionCount = 0;
        inExecute = false;
        doneWithConnection = false;
        connectionCloseForced = false;
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
        } else {
            // Make sure the connection has been released. If the response 
            // stream has not been set, this is the only way to release the 
            // connection. 
            ensureConnectionRelease();
        }
    }

    /**
     * Remove the request header associated with the given name. Note that
     * header-name matching is case insensitive.
     *
     * @param headerName the header name
     */
    public void removeRequestHeader(String headerName) {
        
        Header[] headers = getRequestHeaderGroup().getHeaders(headerName);
        for (int i = 0; i < headers.length; i++) {
            getRequestHeaderGroup().removeHeader(headers[i]);
        }
        
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
        LOG.trace("enter HttpMethodBase.addAuthorizationRequestHeader("
                  + "HttpState, HttpConnection)");

        // add authorization header, if needed
        if (getRequestHeader(HttpAuthenticator.WWW_AUTH_RESP) == null) {
            Header[] challenges = getResponseHeaderGroup().getHeaders(
                                               HttpAuthenticator.WWW_AUTH);
            if (challenges.length > 0) {
                try {
                    AuthScheme authscheme = HttpAuthenticator.selectAuthScheme(challenges);
                    HttpAuthenticator.authenticate(authscheme, this, conn, state);
                } catch (HttpException e) {
                    // log and move on
                    if (LOG.isErrorEnabled()) {
                        LOG.error(e.getMessage(), e);
                    }
                }
            }
        }
    }

    /**
     * Adds a <tt>Content-Length</tt> or <tt>Transfer-Encoding: Chunked</tt>
     * request header, as long as no <tt>Content-Length</tt> request header
     * already exists.
     * 
     * TODO: Revise this method as it is potentially error-prone. 
     * 'Transfer-encoding: chunked' header should not be set here 
     * as some sub classes may not support chunk-encoding.
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
        LOG.trace("enter HttpMethodBase.addContentLengthRequestHeader("
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

        LOG.trace("enter HttpMethodBase.addCookieRequestHeader(HttpState, "
                  + "HttpConnection)");

        // Clean up the cookie headers
        removeRequestHeader("cookie");

        CookieSpec matcher = CookiePolicy.getSpecByPolicy(state.getCookiePolicy());
        Cookie[] cookies = matcher.match(conn.getHost(), conn.getPort(),
            getPath(), conn.isSecure(), state.getCookies());
        if ((cookies != null) && (cookies.length > 0)) {
            if (this.isStrictMode()) {
                // In strict mode put all cookies on the same header
                getRequestHeaderGroup().addHeader(
                  matcher.formatCookieHeader(cookies));
            } else {
                // In non-strict mode put each cookie on a separate header
                for (int i = 0; i < cookies.length; i++) {
                    getRequestHeaderGroup().addHeader(
                      matcher.formatCookieHeader(cookies[i]));
                }
            }
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
        LOG.trace("enter HttpMethodBase.addHostRequestHeader(HttpState, "
                  + "HttpConnection)");

        // Per 19.6.1.1 of RFC 2616, it is legal for HTTP/1.0 based
        // applications to send the Host request-header.
        // TODO: Add the ability to disable the sending of this header for
        //       HTTP/1.0 requests.
        String host = conn.getVirtualHost();
        if (host != null) {
            LOG.debug("Using virtual host name: " + host);
        } else {
            host = conn.getHost();
        }
        int port = conn.getPort();

        if (getRequestHeader("host") != null) {
            LOG.debug(
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
        if (LOG.isDebugEnabled()) {
            LOG.debug("Adding Host request header");
        }

        //appends the port only if not using the default port for the protocol
        if (conn.getProtocol().getDefaultPort() != port) {
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
        LOG.trace("enter HttpMethodBase.addProxyAuthorizationRequestHeader("
                  + "HttpState, HttpConnection)");

        // add proxy authorization header, if needed
        if (getRequestHeader(HttpAuthenticator.PROXY_AUTH_RESP) == null) {
            Header[] challenges = getResponseHeaderGroup().getHeaders(
                                               HttpAuthenticator.PROXY_AUTH);
            if (challenges.length > 0) {
                try {
                    AuthScheme authscheme = HttpAuthenticator.selectAuthScheme(challenges);
                    HttpAuthenticator.authenticateProxy(authscheme, this, conn, state);
                } catch (HttpException e) {
                    // log and move on
                    if (LOG.isErrorEnabled()) {
                        LOG.error(e.getMessage(), e);
                    }
                }
            }
        }
    }

    /**
     * Adds a <tt>Proxy-Connection: Keep-Alive</tt> request when 
     * communicating via a proxy server.
     *
     * @param state current state of http requests
     * @param conn the connection to use for I/O
     *
     * @throws IOException when errors occur reading or writing to/from the
     *         connection
     * @throws HttpException when a recoverable error occurs
     */
    protected void addProxyConnectionHeader(HttpState state,
                                            HttpConnection conn)
    throws IOException, HttpException {
        LOG.trace("enter HttpMethodBase.addProxyConnectionHeader("
                  + "HttpState, HttpConnection)");
        if (!conn.isTransparent()) {
            setRequestHeader("Proxy-Connection", "Keep-Alive");
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
        LOG.trace("enter HttpMethodBase.addRequestHeaders(HttpState, "
            + "HttpConnection)");

        addUserAgentRequestHeader(state, conn);
        addHostRequestHeader(state, conn);
        addCookieRequestHeader(state, conn);
        addAuthorizationRequestHeader(state, conn);
        addProxyAuthorizationRequestHeader(state, conn);
        addProxyConnectionHeader(state, conn);
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
        LOG.trace("enter HttpMethodBase.addUserAgentRequestHeaders(HttpState, "
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
     * @param version the protocol version to use (e.g. HTTP/1.0)
     *
     * @return a line to send to the server that will fulfil the request
     */
    protected static String generateRequestLine(HttpConnection connection,
        String name, String requestPath, String query, String version) {
        LOG.trace("enter HttpMethodBase.generateRequestLine(HttpConnection, "
            + "String, String, String, String)");

        StringBuffer buf = new StringBuffer();
        // Append method name
        buf.append(name);
        buf.append(" ");
        // Absolute or relative URL?
        if (!connection.isTransparent()) {
            Protocol protocol = connection.getProtocol();
            buf.append(protocol.getScheme().toLowerCase());
            buf.append("://");
            buf.append(connection.getHost());
            if ((connection.getPort() != -1) 
                && (connection.getPort() != protocol.getDefaultPort())
            ) {
                buf.append(":");
                buf.append(connection.getPort());
            }
        }
        // Append path, if any
        if (requestPath == null) {
            buf.append("/");
        } else {
            if (!connection.isTransparent() && !requestPath.startsWith("/")) {
                buf.append("/");
            }
            buf.append(requestPath);
        }
        // Append query, if any
        if (query != null) {
            if (query.indexOf("?") != 0) {
                buf.append("?");
            }
            buf.append(query);
        }
        // Append protocol
        buf.append(" ");
        buf.append(version);
        buf.append("\r\n");
        
        return buf.toString();
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
        LOG.trace("enter HttpMethodBase.processResponseHeaders(HttpState, "
            + "HttpConnection)");

        Header[] headers = getResponseHeaderGroup().getHeaders("set-cookie2");
        //Only process old style set-cookie headers if new style headres
        //are not present
        if (headers.length == 0) { 
            headers = getResponseHeaderGroup().getHeaders("set-cookie");
        }
        
        CookieSpec parser = CookiePolicy.getSpecByPolicy(state.getCookiePolicy());
        for (int i = 0; i < headers.length; i++) {
            Header header = headers[i];
            Cookie[] cookies = null;
            try {
                cookies = parser.parse(
                  conn.getHost(),
                  conn.getPort(),
                  getPath(),
                  conn.isSecure(),
                  header);
            } catch (MalformedCookieException e) {
                if (LOG.isWarnEnabled()) {
                    LOG.warn("Invalid cookie header: \"" 
                        + header.getValue() 
                        + "\". " + e.getMessage());
                }
            }
            if (cookies != null) {
                for (int j = 0; j < cookies.length; j++) {
                    Cookie cookie = cookies[j];
                    try {
                        parser.validate(
                          conn.getHost(),
                          conn.getPort(),
                          getPath(),
                          conn.isSecure(),
                          cookie);
                        state.addCookie(cookie);
                        if (LOG.isDebugEnabled()) {
                            LOG.debug("Cookie accepted: \"" 
                                + parser.formatCookie(cookie) + "\"");
                        }
                    } catch (MalformedCookieException e) {
                        if (LOG.isWarnEnabled()) {
                            LOG.warn("Cookie rejected: \"" + parser.formatCookie(cookie) 
                                + "\". " + e.getMessage());
                        }
                    }
                }
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
        LOG.trace(
            "enter HttpMethodBase.readResponse(HttpState, HttpConnection)");
        try {
            // Status line & line may have already been received
            // if 'expect - continue' handshake has been used
            while (this.statusLine == null) {
                readStatusLine(state, conn);
                processStatusLine(state, conn);
                readResponseHeaders(state, conn);
                processResponseHeaders(state, conn);
                
                int status = this.statusLine.getStatusCode();
                if ((status >= 100) && (status < 200)) {
                    if (LOG.isInfoEnabled()) {
                        LOG.info("Discarding unexpected response: " + this.statusLine.toString()); 
                    }
                    this.statusLine = null;
                }
            }
            readResponseBody(state, conn);
            processResponseBody(state, conn);
        } catch (IOException e) {
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
        LOG.trace(
            "enter HttpMethodBase.readResponseBody(HttpState, HttpConnection)");

        // assume we are not done with the connection if we get a stream
        doneWithConnection = false;
        InputStream stream = readResponseBody(conn);
        if (stream == null) {
            // done using the connection!
            responseBodyConsumed();
        } else {
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
     * @throws IOException if an IO problem occurs.
     */
    private InputStream readResponseBody(HttpConnection conn)
        throws IOException {

        LOG.trace("enter HttpMethodBase.readResponseBody(HttpState, HttpConnection)");

        responseBody = null; // is this desired?
        InputStream is = conn.getResponseInputStream();
        if (Wire.enabled()) {
            is = new WireLogInputStream(is);
        }
        InputStream result = null;
        Header transferEncodingHeader = responseHeaders.getFirstHeader("Transfer-Encoding");
        // We use Transfer-Encoding if present and ignore Content-Length.
        // RFC2616, 4.4 item number 3
        if (transferEncodingHeader != null) {

            String transferEncoding = transferEncodingHeader.getValue();
            if (!"chunked".equalsIgnoreCase(transferEncoding) && 
                !"identity".equalsIgnoreCase(transferEncoding)) {
                if (LOG.isWarnEnabled()) {
                    LOG.warn("Unsupported transfer encoding: " + transferEncoding);
                }
            }
            HeaderElement[] encodings = transferEncodingHeader.getValues();
            // The chunck encoding must be the last one applied
            // RFC2616, 14.41
            int len = encodings.length;            
            if ((len > 0) && ("chunked".equalsIgnoreCase(encodings[len - 1].getName() ))) { 
                // if response body is empty
                if (conn.isResponseAvailable(conn.getSoTimeout())) {
                    result = new ChunkedInputStream(is, this);
                } else {
                    if (isStrictMode()) {
                        throw new HttpException("Chunk-encoded body declared but not sent");
                    } else {
                        LOG.warn("Chunk-encoded body missing");
                    }
                }
            } else {
                if (isStrictMode() && LOG.isWarnEnabled()) {
                    LOG.warn("Transfer-Encoding is set but does not contain \"chunked\": "
                        + transferEncoding);
                }
                // The connection must be terminated by closing 
                // the socket as per RFC 2616, 3.6
                setConnectionCloseForced(true);
                result = is;  
            }
        } else {
            int expectedLength = getResponseContentLength();
            if (expectedLength == -1) {
                if (canResponseHaveBody(statusLine.getStatusCode())) {
                    setConnectionCloseForced(true);
                    result = is;            
                }
            } else {
                result = new ContentLengthInputStream(is, expectedLength);
            }
        } 
        // if there is a result - ALWAYS wrap it in an observer which will
        // close the underlying stream as soon as it is consumed, and notify
        // the watcher that the stream has been consumed.
        if (result != null) {

            result = new AutoCloseInputStream(
                result,
                new ResponseConsumedWatcher() {
                    public void responseConsumed() {
                        responseBodyConsumed();
                    }
                }
            );
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
        LOG.trace("enter HttpMethodBase.readResponseHeaders(HttpState,"
            + "HttpConnection)");

        getResponseHeaderGroup().clear();
        Header[] headers = HttpParser.parseHeaders(conn.getResponseInputStream());
        if (Wire.enabled()) {
            for (int i = 0; i < headers.length; i++) {
                Wire.input(headers[i].toExternalForm());
            }
        }
        getResponseHeaderGroup().setHeaders(headers);
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
        LOG.trace("enter HttpMethodBase.readStatusLine(HttpState, HttpConnection)");

        //read out the HTTP status string
        String statusString = conn.readLine();
        while ((statusString != null) && !statusString.startsWith("HTTP")) {
            if (Wire.enabled()) {
                Wire.input(statusString + "\r\n");
            }
            statusString = conn.readLine();
        }
        if (statusString == null) {
            // A null statusString means the connection was lost before we got a
            // response.  Try again.
            throw new HttpRecoverableException("Error in parsing the status "
                + " line from the response: unable to find line starting with"
                + " \"HTTP\"");
        }
        if (Wire.enabled()) {
            Wire.input(statusString + "\r\n");
        }
        //create the status line from the status string
        statusLine = new StatusLine(statusString);

        //check for a valid HTTP-Version
        String httpVersion = statusLine.getHttpVersion();
        if (httpVersion.equals("HTTP/1.0")) {
            http11 = false;
        } else if (httpVersion.equals("HTTP/1.1")) {
            http11 = true;
        } else if (httpVersion.equals("HTTP")) {
            // some servers do not specify the version correctly, we will just assume 1.0
            http11 = false;
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
        LOG.trace(
            "enter HttpMethodBase.writeRequest(HttpState, HttpConnection)");
        writeRequestLine(state, conn);
        writeRequestHeaders(state, conn);
        conn.writeLine(); // close head
        // make sure the status line and headers have been sent
        conn.flushRequestOutputStream();
        if (Wire.enabled()) {
            Wire.output("\r\n");
        }

        Header expectheader = getRequestHeader("Expect");
        String expectvalue = null;
        if (expectheader != null) {
            expectvalue = expectheader.getValue();
        }
        if ((expectvalue != null) 
         && (expectvalue.compareToIgnoreCase("100-continue") == 0)) {
            if (this.isHttp11()) {
                int readTimeout = conn.getSoTimeout();
                try {
                    conn.setSoTimeout(RESPONSE_WAIT_TIME_MS);
                    readStatusLine(state, conn);
                    processStatusLine(state, conn);
                    readResponseHeaders(state, conn);
                    processResponseHeaders(state, conn);

                    if (this.statusLine.getStatusCode() == HttpStatus.SC_CONTINUE) {
                        // Discard status line
                        this.statusLine = null;
                        LOG.debug("OK to continue received");
                    } else {
                        return;
                    }
                } catch (InterruptedIOException e) {
                    // Most probably Expect header is not recongnized
                    // Remove the header to signal the method 
                    // that it's okay to go ahead with sending data
                    removeRequestHeader("Expect");
                    LOG.info("100 (continue) read timeout. Resume sending the request");
                } finally {
                    conn.setSoTimeout(readTimeout);
                }
                
            } else {
                removeRequestHeader("Expect");
                LOG.info("'Expect: 100-continue' handshake is only supported by "
                    + "HTTP/1.1 or higher");
            }
        }

        writeRequestBody(state, conn);
        // make sure the entire request body has been sent
        conn.flushRequestOutputStream();
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
        LOG.trace("enter HttpMethodBase.writeRequestHeaders(HttpState,"
            + "HttpConnection)");
        addRequestHeaders(state, conn);

        Header[] headers = getRequestHeaders();
        for (int i = 0; i < headers.length; i++) {
            String s = headers[i].toExternalForm();
            if (Wire.enabled()) {
                Wire.output(s);
            }
            conn.print(s);
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
        LOG.trace(
            "enter HttpMethodBase.writeRequestLine(HttpState, HttpConnection)");
        String requestLine = getRequestLine(conn);
        if (Wire.enabled()) {
            Wire.output(requestLine);
        }
        conn.print(requestLine);
    }

    /**
     * Gets the request line that was sent to the http server.
     * Consider making this public.  Consider creating a new class
     * RequestLine for this purpose.
     * 
     * @param conn The http connection
     * @return The request line.
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
     * Per RFC 2616 section 4.3, some response can never contain a message
     * body.
     *
     * @param status - the HTTP status code
     *
     * @return true if the message may contain a body, false if it can not
     *         contain a message body
     */
    private static boolean canResponseHaveBody(int status) {
        LOG.trace("enter HttpMethodBase.canResponseHaveBody(int)");

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
     * @param conn The connection
     *
     * @return true if the request has completed process, false if more
     *         attempts are needed
     */
    private boolean processAuthenticationResponse(HttpState state, HttpConnection conn) {
        LOG.trace("enter HttpMethodBase.processAuthenticationResponse("
            + "HttpState, HttpConnection)");

        int statusCode = statusLine.getStatusCode();
        // handle authentication required
        Header[] challenges = null;
        Set realmsUsed = null;
        switch (statusCode) {
            case HttpStatus.SC_UNAUTHORIZED:
                challenges = getResponseHeaderGroup().getHeaders(HttpAuthenticator.WWW_AUTH);
                realmsUsed = realms;
                break;
            case HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED:
                challenges = getResponseHeaderGroup().getHeaders(HttpAuthenticator.PROXY_AUTH);
                realmsUsed = proxyRealms;
                break;
        }
        boolean authenticated = false;
        // if there was a header requesting authentication
        if (challenges.length > 0) {
            AuthScheme authscheme = null;
            try {
                authscheme = HttpAuthenticator.selectAuthScheme(challenges);
            } catch (MalformedChallengeException e) {
                if (LOG.isErrorEnabled()) {
                    LOG.error(e.getMessage(), e);
                }
                return true;
            } catch (UnsupportedOperationException e) {
                if (LOG.isErrorEnabled()) {
                    LOG.error(e.getMessage(), e);
                }
                return true;
            }
        
            StringBuffer buffer = new StringBuffer();
            buffer.append(conn.getHost());
            int port = conn.getPort();
            if (conn.getProtocol().getDefaultPort() != port) {
                buffer.append(':');
                buffer.append(port);
            }
            buffer.append('#');
            buffer.append(authscheme.getID());
            String realm = buffer.toString();

            if (realmsUsed.contains(realm)) {
                if (LOG.isInfoEnabled()) {
                    LOG.info("Already tried to authenticate to \""
                             + realm + "\" but still receiving "
                             + statusCode + ".");
                }
                return true;
            } else {
                realmsUsed.add(realm);
            }

            try {
                //remove preemptive header and reauthenticate
                switch (statusCode) {
                    case HttpStatus.SC_UNAUTHORIZED:
                        removeRequestHeader(HttpAuthenticator.WWW_AUTH_RESP);
                        authenticated = HttpAuthenticator.authenticate(
                            authscheme, this, conn, state);
                        this.realm = authscheme.getRealm();
                        break;
                    case HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED:
                        removeRequestHeader(HttpAuthenticator.PROXY_AUTH_RESP);
                        authenticated = HttpAuthenticator.authenticateProxy(
                            authscheme, this, conn, state);
                        this.proxyRealm = authscheme.getRealm();
                        break;
                }
            } catch (AuthenticationException e) {
                LOG.warn(e.getMessage());
                return true; // finished request
            }
            if (!authenticated) {
                // won't be able to authenticate to this challenge
                // without additional information
                LOG.debug("HttpMethodBase.execute(): Server demands "
                          + "authentication credentials, but none are "
                          + "available, so aborting.");
            } else {
                LOG.debug("HttpMethodBase.execute(): Server demanded "
                          + "authentication credentials, will try again.");
                // let's try it again, using the credentials
            }
        }

        return !authenticated; // finished processing if we aren't authenticated
    }

    /**
     * Returns proxy authentication realm, if it has been used during authentication process. 
     * Otherwise returns <tt>null</tt>.
     * 
     * @return proxy authentication realm
     */
    public String getProxyAuthenticationRealm() {
        return this.proxyRealm;
    }

    /**
     * Returns authentication realm, if it has been used during authentication process. 
     * Otherwise returns <tt>null</tt>.
     * 
     * @return authentication realm
     */
    public String getAuthenticationRealm() {
        return this.realm;
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
        LOG.trace("enter HttpMethodBase.processRequest(HttpState, HttpConnection)");

        int execCount = 0;
        boolean requestSent = false;
        
        // loop until the method is successfully processed, the retryHandler 
        // returns false or a non-recoverable exception is thrown
        while (true) {
            execCount++;
            requestSent = false;
            
            if (LOG.isTraceEnabled()) {
                LOG.trace("Attempt number " + execCount + " to process request");
            }
            try {
                if (!connection.isOpen()) {
                    LOG.debug("Opening the connection.");
                    connection.open();
                }
                writeRequest(state, connection);
                requestSent = true;
                readResponse(state, connection);
                // the method has successfully executed
                used = true; 
                break;
            } catch (HttpRecoverableException httpre) {
                if (LOG.isDebugEnabled()) {
                    LOG.debug("Closing the connection.");
                }
                connection.close();
                LOG.info("Recoverable exception caught when processing request");
                // update the recoverable exception count.
                recoverableExceptionCount++;
                
                // test if this method should be retried                
                if (!getMethodRetryHandler().retryMethod(
                        this, 
                        connection, 
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
    }

    /**
     * Return the character set from the header.
     * @param contentheader The content header.
     * @return String The character set.
     */
    protected static String getContentCharSet(Header contentheader) {
        LOG.trace("enter getContentCharSet( Header contentheader )");
        String charset = null;
        if (contentheader != null) {
            try {
                HeaderElement values[] = contentheader.getValues();
                // I expect only one header element to be there
                // No more. no less
                if (values.length == 1) {
                    NameValuePair param = values[0].getParameterByName("charset");
                    if (param != null) {
                        // If I get anything "funny" 
                        // UnsupportedEncondingException will result
                        charset = param.getValue();
                    }
                }
            } catch (HttpException e) {
                LOG.error(e);
            }
        }
        if (charset == null) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Default charset used: " + HttpConstants.DEFAULT_CONTENT_CHARSET);
            }
            charset = HttpConstants.DEFAULT_CONTENT_CHARSET;
        }
        return charset;
    }


    /**
     * Return the character set for the request.  This is determined from the
     * "Content-Type" header.
     * @return String The character set.
     */
    public String getRequestCharSet() {
        return getContentCharSet(getRequestHeader("Content-Type"));
    }


    /**  
     * Return the character set for the response.  This is determined from the
     * "Content-Type" header.
     * @return String The character set.
     */
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

        if (shouldCloseConnection(responseConnection)) {
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
        if (responseConnection != null) {
            responseConnection.releaseConnection();
            responseConnection = null;
        }
    }

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

    /**
     * @return the methodRetryHandler
     */
    public MethodRetryHandler getMethodRetryHandler() {
        
        if (methodRetryHandler == null) {
            methodRetryHandler = new DefaultMethodRetryHandler();
        }

        return methodRetryHandler;
    }

    /**
     * @param handler the methodRetryHandler to use when this method executed
     */
    public void setMethodRetryHandler(MethodRetryHandler handler) {
        methodRetryHandler = handler;
    }

    /**
     * This method is a dirty hack intended to work around 
     * current (2.0) design flaw that prevents the user from
     * obtaining correct status code, headers and response body from the 
     * preceding HTTP CONNECT method.
     * 
     * TODO: Remove this crap as soon as possible
     */
    protected void fakeResponse(
        StatusLine statusline, 
        HeaderGroup responseheaders,
        InputStream responseStream
    ) {
        // set used so that the response can be read
        this.used = true;
        this.statusLine = statusline;
        this.responseHeaders = responseheaders;
        this.responseBody = null;
        this.responseStream = responseStream;
    }
}
