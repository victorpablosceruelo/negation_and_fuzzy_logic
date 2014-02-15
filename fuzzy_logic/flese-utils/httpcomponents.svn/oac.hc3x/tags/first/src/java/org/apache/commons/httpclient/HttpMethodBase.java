/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpMethodBase.java,v 1.1 2001/04/25 18:42:51 remm Exp $
 * $Revision: 1.1 $
 * $Date: 2001-04-25 20:42:48 +0200 (Wed, 25 Apr 2001) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Tomcat", and "Apache Software
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

import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Vector;
import java.util.Hashtable;


/**
 * HTTP method abstract implementation.
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 */
public abstract class HttpMethodBase
    implements HttpMethod {


    // ----------------------------------------------------- Instance Variables


    /**
     * True if this method has already been executed.
     */
    private boolean used = false;


    /**
     * Debug.
     */
    protected int debug = 0;


    /**
     * Status code.
     */
    protected int statusCode = HttpStatus.SC_OK;


    /**
     * Status text.
     */
    protected String statusText = "OK";


    /**
     * Server path.
     */
    protected String path = "/";


    /**
     * Method name.
     */
    protected String name;


    /**
     * Request headers.
     */
    protected Hashtable requestHeaders = new Hashtable();


    /**
     * Response headers.
     */
    protected Hashtable responseHeaders = new Hashtable();


    /**
     * URL parameters.
     */
    protected Hashtable parameters = new Hashtable();


    /**
     * Global state.
     */
    protected State state;


    /*
     * Holds the query body if set by setQuery.
     */
    protected String query = null;


    /*
     * Holds the query string.
     */
    protected String queryString = null;


    // ----------------------------------------------------------- Constructors
    

    /**
     * Method constructor.
     */
    public HttpMethodBase() {
    }


    /**
     * Method constructor.
     */
    public HttpMethodBase(String path) {
        setPath(path);
    }


    // ------------------------------------------------------------- Properties


    /**
     * Debug property setter.
     *
     * @param int Debug
     */
    public void setDebug(int debug) {
        this.debug = debug;
    }


    /**
     * Status code property setter.
     *
     * @param int Status code
     */
    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }


    /**
     * Status code property getter.
     *
     * @return int Status code
     */
    public int getStatusCode() {
        return statusCode;
    }


    /**
     * Status text property setter.
     *
     * @param statusText Status text
     */
    public void setStatusText(String statusText) {
        this.statusText = statusText;
    }


    /**
     * Status text property getter.
     *
     * @return String status text
     */
    public String getStatusText() {
        checkUsed();
        return statusText;
    }


    /**
     * Path property setter.
     *
     * @param path Absolute path
     */
    public void setPath(String path) {
        checkNotUsed();
        if ((path == null) || (path.equals(""))) {
            this.path = "/";
        } else {
            this.path = path;
        }
    }


    /**
     * Path property getter.
     *
     * @return String path
     */
    public String getPath() {
        return path;
    }


    /**
     * Name property getter.
     *
     * @return String name
     */
    public String getName() {
        return name;
    }


    /**
     * Set header.
     *
     * @param headerName Header name
     * @param headerValue Header value
     */
    public void setHeader(String headerName, String headerValue) {
        getHeadersHashtable().put(headerName.toLowerCase(),
                                  new Header(headerName, headerValue));
    }


    /**
     * Get header.
     *
     * @param headerName Header name
     * @return String header value (null if the header doesn't exist)
     */
    public Header getHeader(String headerName) {
        return (Header) getHeadersHashtable().get(headerName.toLowerCase());
    }


    /**
     * Remove header.
     *
     * @param headerName Header name
     */
    public void removeHeader(String headerName) {
        getHeadersHashtable().remove(headerName.toLowerCase());
    }


    /**
     * Checks if this method's instance has already been used, and has not been
     * recycled.
     *
     * @return boolean True if the method's instance has already been used
     */
    public final boolean hasBeenUsed() {
        return (used);
    }


    /**
     * True if this methods should automatically follow redirects.
     *
     * @return boolean True if auto redirect should be used for this method
     */
    public boolean followRedirects() {
        return (false);
    }


    /**
     * Set the state token.
     */
    public void setState(State state) {
        this.state = state;
    }


    /**
     * Set URL parameter.
     *
     * @param parameterName Parameter name
     * @param parameterValue Parameter value
     */
    public void setParameter(String parameterName, String parameterValue) {
        parameters.put(parameterName, parameterValue);
    }


    /**
     * Set query string.
     *
     * @param queryString Query string
     */
    public void setQueryString(String queryString) {
        this.queryString = queryString;
    }


    // ----------------------------------------------------- HttpMethod Methods


    /**
     * Set the method as used.
     */
    public void setUsed() {
        used = true;
    }


    /**
     * Ensures the correctness of the request according to criterions which are
     * method dependent.
     *
     * @return boolean True if the method is valid
     */
    public boolean validate() {
        // By default, the request is valid.
        return (true);
    }


    /**
     * Recycle the method object, so that it can be reused again. Any attempt
     * to reuse an object without recycling it will throw a HttpException.
     */
    public void recycle() {
        path = "/";
        statusCode = HttpStatus.SC_OK;
        statusText = "OK";
        requestHeaders.clear();
        responseHeaders.clear();
        parameters.clear();
        state = null;
        used = false;
        query = null;
        queryString = null;
    }


    /**
     * Get headers.
     *
     * @return Enumeration
     */
    public Enumeration getHeaders() {
        return getHeadersHashtable().elements();
    }


    /**
     * Generate additional headers needed by the request.
     *
     * @param state State token
     * @deprecated this method is deprecated in favour of the
     * <CODE>generateHeaders(String, State)</CODE> method.
     */
    public void generateHeaders(State state) {

        generateHeaders("default", state);

    }


    /**
     * Generate additional headers needed by the request.
     *
     * @param host the host
     * @param state State token
     */
    public void generateHeaders(String host, State state) {

        responseHeaders.clear();

        // good practice to provide a user-agent indicator
        if (!requestHeaders.containsKey("user-agent")) {
            requestHeaders.put("user-agent", HttpClient.USER_AGENT);
        }

        if (!requestHeaders.containsKey("host")) {
            requestHeaders.put("host", new Header("Host", host));
        }

        // add the cookies
        if (!requestHeaders.containsKey("cookie")) {
            Vector cookies = state.getCookies();
            if (cookies != null && cookies.size() > 0) {
                requestHeaders.put
                    ("cookie", Cookie.createCookieHeader
                     (host, getPath(), cookies));
            }
        }

    }


    /**
     * Is the query body submitted through an InputStream of with a String.
     * If an InputStream is available, it's used.
     *
     * @return boolean True if the content is avalable in an InputStream
     */
    public boolean isStreamedQuery() {
        // By default, the query is NOT streamed.
        return false;
    }


    /**
     * Set the query String as a XML document.
     * If the query string is already given as an XML document
     * this low level function will set the query string directly.
     * This method make only sense in the context of lock, propFind,
     * and propPatch. In all other cases this method call is ignored.
     */
    public void setQuery(String query) {
        this.query = query;
    }


    /**
     * Generate the query body.
     *
     * @return String query
     */
    public String generateQuery() {
        return query!=null?query:"";
    }


    /**
     * Stream the body of the query. This function should be used to send large
     * request bodies.
     */
    public void streamQuery(OutputStream out)
        throws IOException {

    }


    /**
     * Process response headers. The contract of this method is that it only
     * parses the response headers.
     *
     * @param headers Headers list
     */
    public void processResponseHeaders(Hashtable headers) {

        // We replace the request headers with the response headers
        this.responseHeaders = headers;

    }


    /**
     * Parse response.
     *
     * @param is Input stream
     */
    public abstract void parseResponse(InputStream is)
        throws IOException, HttpException;


    /**
     * Generate the HTTP request line.
     *
     * @return String request line
     */
    public final String generateRequestLine() {

        String requestString = state.URLEncode(path);
        if (queryString != null) {
            if (queryString.indexOf("?") < 0) {
                requestString += "?";
            }
            requestString += queryString;
        } else if (!parameters.isEmpty()) {
            // Parsing parameters list
            StringBuffer parametersString = new StringBuffer();
            Enumeration paramNames = parameters.keys();
            while (paramNames.hasMoreElements()) {
                String paramName = (String) paramNames.nextElement();
                String paramValue = (String) parameters.get(paramName);
                parametersString.append(state.URLEncode(paramName));
                parametersString.append("=");
                parametersString.append(state.URLEncode(paramValue));
                if (paramNames.hasMoreElements()) {
                    parametersString.append("&");
                }
            }
            requestString = requestString + "?" + parametersString.toString();
        }

        return (getName() + " " + requestString + " " + PROTOCOL + "\r\n");

    }


    /**
     * Return true if the method needs a content-length header in the request.
     *
     * @return true if a content-length header will be expected by the server
     */
    public boolean needContentLength() {
        return true;
    }


    /**
     * Return true if the method should ask for an expectation.
     * 
     * @return true if an expectation will be sent
     */
    public boolean needExpectation() {
        return false;
    }


    // ------------------------------------------------------ Protected Methods


    /**
     * Returns the active headers hashtable.
     */
    protected Hashtable getHeadersHashtable() {
        if (hasBeenUsed()) {
            return responseHeaders;
        } else {
            return requestHeaders;
        }
    }


    /**
     * Check if the method has been executed, and throws an
     * IllegalStateException otherwise.
     */
    protected void checkUsed() {
        if (!hasBeenUsed())
            throw new IllegalStateException
                ("Method must be executed before this function is called");
    }


    /**
     * Check if the method has been executed, and throws an
     * IllegalStateException otherwise.
     */
    protected void checkNotUsed() {
        if (hasBeenUsed())
            throw new IllegalStateException
                ("This function must be called before this method "
                 + "is executed");
    }


}
