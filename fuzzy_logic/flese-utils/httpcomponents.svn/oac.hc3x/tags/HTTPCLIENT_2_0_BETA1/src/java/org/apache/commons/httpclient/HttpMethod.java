/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpMethod.java,v 1.23 2003/01/28 04:40:20 jsdever Exp $
 * $Revision: 1.23 $
 * $Date: 2003-01-28 05:40:23 +0100 (Tue, 28 Jan 2003) $
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

import java.io.IOException;
import java.io.InputStream;

/**
 * <p>
 * A request to be applied to an {@link HttpConnection}, and a container for the
 * associated response.
 * </p>
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Rod Waldhoff
 * @author <a href="jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 *
 * @version $Revision: 1.23 $ $Date: 2003-01-28 05:40:23 +0100 (Tue, 28 Jan 2003) $
 * @since 1.0
 */
public interface HttpMethod {

    // ------------------------------------------- Property Setters and Getters

    /**
     * Obtain the name of this method, suitable for use in the "request line",
     * for example <tt>"GET"</tt> or <tt>"POST"</tt>.
     * @return the name of this method
     */
    String getName();

    /**
     * Gets the host configuration for this method.
     * 
     * @return the HostConfiguration or <code>null</code> if none is set
     */
    HostConfiguration getHostConfiguration();

    /**
     * Set the path part of my request.
     * @param path the path to request
     */
    void setPath(String path);

    /**
     * Get the path part of my request.  
     *
     * Calling this method AFTER the request
     * has been executed will return the ACTUAL path, following any 301 or 302 
     * redirects (except for redirects off the initial server, which are not 
     * supported, or when HttpClient is set to not follow redirects; in either
     * case, HttpClient returns the 301 or 302 status code).
     * @return the path to request
     */
    String getPath();

    /**
     * Gets the URI for this method.  The URI will be absolute if the host
     * configuration has been set or relative otherwise.
     * 
     * @return URI 
     * 
     * @throws URIException if a URI cannot be constructed
     */
    URI getURI() throws URIException;

    /**
     * <p>Turns strict mode on or off.  In strict mode (the default) we
     * following the letter of RFC 2616, the Http 1.1 specification. If strict
     * mode is turned off we attempt to violate the specification in the same
     * way that most Http user agent's do (and many HTTP servers expect.</p>
     *
     * <p>NOTE:  StrictMode is currently experimental and its functionaity may
     * change in the future.</p>
     * 
     * @param strictMode True to enable strict mode.
     */
    void setStrictMode(boolean strictMode);

    /**
     * Returns the value of strictMode.
     *
     * NOTE:  StrictMode is currently experimental and its functionlaity may change in the future.
     *
     * @return true if strict mode is enabled.
     */
    boolean isStrictMode();
     
    /**
     * Set the specified request header, overwriting any
     * previous value.
     * Note that header-name matching is case insensitive.
     * @param headerName the header's name
     * @param headerValue the header's value
     */
    void setRequestHeader(String headerName, String headerValue);

    /**
     * Set the specified request header, overwriting any
     * previous value.
     * Note that header-name matching is case insensitive.
     * @param header the header
     */
    void setRequestHeader(Header header);

    /**
     * Adds the specified request header, NOT overwriting any
     * previous value.
     * Note that header-name matching is case insensitive.
     * @param headerName the header's name
     * @param headerValue the header's value
     */
    void addRequestHeader(String headerName, String headerValue);

    /**
     * Adds the specified request header, NOT overwriting any
     * previous value.
     * Note that header-name matching is case insensitive.
     * @param header the header
     */
    void addRequestHeader(Header header);

    /**
     * Get the request header associated with the given name.
     * Note that header-name matching is case insensitive.
     * @param headerName the header name
     * @return the header
     */
    Header getRequestHeader(String headerName);

    /**
     * Remove all request headers associated with the given name.
     * Note that header-name matching is case insensitive.
     * @param headerName the header name
     */
    void removeRequestHeader(String headerName);

    /**
     * Whether or not I should automatically follow
     * HTTP redirects (status code 302, etc.)
     * @return <tt>true</tt> if I will automatically follow HTTP redirects
     */
    boolean getFollowRedirects();

    /**
     * Set whether or not I should automatically follow HTTP redirects (status
     * code 302, etc.)
     * @param followRedirects True if I should automatically follow redirects.
     */
    void setFollowRedirects(boolean followRedirects);

    /**
     * Set my query string.
     * @param queryString the query string
     */
    void setQueryString(String queryString);

    /**
     * Set my query string.
     * @param params an array of {@link NameValuePair}s
     *               to add as query string parameterss
     */
    void setQueryString(NameValuePair[] params);

    /**
     * Get my query string.
     * @return my query string
     */
    String getQueryString();

    /**
     * Return an array of my request headers.
     * @return an array of request headers.
     */
    Header[] getRequestHeaders();

    // ---------------------------------------------------------------- Queries

    /**
     * Confirm that I am ready to execute.
     * @return True if I am ready to execute.
     */
    boolean validate();

    /**
     * Return the status code associated with the latest response.
     * @return the status code.
     */
    int getStatusCode();

    /**
     * Return the status text (or "reason phrase") associated with the latest
     * response.
     * @return The status text.
     */
    String getStatusText();

    /**
     * Return an array of my response headers.
     * @return An array of all the response headers.
     */
    Header[] getResponseHeaders();

    /**
     * Return the specified response header. Note that header-name matching is
     * case insensitive.
     * @param headerName The name of the header to be returned.
     * @return The specified response header.
     */
    Header getResponseHeader(String headerName);

    /**
     * Return an array of my response footers
     * @return <tt>null</tt> if no footers are available
     */
    Header[] getResponseFooters();

    /**
     * Return the specified response footer. Note that footer-name matching is
     * case insensitive.
     * @param footerName The name of the footer.
     * @return The response footer.
     */
    Header getResponseFooter(String footerName);

    /**
     * Return my response body, if any, as a byte array. Otherwise return
     * <tt>null</tt>.
     * @return The response body.
     */
    byte[] getResponseBody();

    /**
     * Return my response body, if any, as a {@link String}. Otherwise return
     * <tt>null</tt>.
     * @return response body.
     */
    String getResponseBodyAsString();

    /**
     * Return my response body, if any, as an {@link InputStream}. Otherwise
     * return <tt>null</tt>.
     * @return As above.
     * @throws IOException If an IO problem occurs.
     */
    InputStream getResponseBodyAsStream() throws IOException;

    /**
     * Return <tt>true</tt> if I have been {@link #execute executed}
     * but not recycled.
     * @return true if this has been used.
     */
    boolean hasBeenUsed();

    // --------------------------------------------------------- Action Methods

    /**
     * Execute this method.
     *
     * @param state state information to associate with this request
     * @param connection the {@link HttpConnection} to write to/read from
     *
     * @throws IOException if an I/O error occurs
     * @throws HttpException  if an protocol exception occurs
     *
     * @return the integer status code if one was obtained, or <tt>-1</tt>
     */
    int execute(HttpState state, HttpConnection connection) 
        throws HttpException, IOException;

    /**
     * Recycle this method so that it can be used again.
     * Note that all of my instance variables will be reset
     * once this method has been called.
     */
    void recycle();

    /**
     * Releases the connection being used by this method.  In particular the
     * connection is used to read the response(if there is one) and will be held
     * until the response has been read.
     */
    void releaseConnection();

    /**
     * Use this method internally to add footers.
     * @param footer The footer to add.
     * @since 2.0
     */
    void addResponseFooter(Header footer);

    /** 
     * Return the Status-Line from the response.
     * @return The status line
     * @since 2.0
     */
    StatusLine getStatusLine();

    /**
     * Whether or not I should automatically process responses where
     * authentication is required (status code 401, etc.)
     *
     * @return <tt>true</tt> if authentications will be processed automatically
     * @since 2.0
     */
    boolean getDoAuthentication();

    /**
     * Set whether or not I should automatically process responses where
     * authentication is required (status code 401, etc.)
     *
     * @param doAuthentication <tt>true</tt> to process authentications
     * @since 2.0
     */
    void setDoAuthentication(boolean doAuthentication);
      
}
