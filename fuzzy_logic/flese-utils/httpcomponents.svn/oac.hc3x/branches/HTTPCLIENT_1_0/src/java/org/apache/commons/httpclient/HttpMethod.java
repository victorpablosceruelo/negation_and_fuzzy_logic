/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpMethod.java,v 1.7 2001/09/01 21:40:45 remm Exp $
 * $Revision: 1.7 $
 * $Date: 2001-09-01 23:40:46 +0200 (Sat, 01 Sep 2001) $
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
import java.util.Hashtable;

/**
 * HTTP method.
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 */
public interface HttpMethod {


    // -------------------------------------------------------------- Constants


    /**
     * Protocol version.
     */
    public static final String PROTOCOL = "HTTP/1.1";


    // ------------------------------------------------------------- Properties


    /**
     * Debug property setter.
     *
     * @param int Debug
     */
    public void setDebug(int debug);


    /**
     * Set the method as used.
     */
    public void setUsed();


    /**
     * Status code property setter.
     *
     * @param int Status code
     */
    public void setStatusCode(int statusCode);


    /**
     * Status code property getter.
     *
     * @return int Status code
     */
    public int getStatusCode();


    /**
     * Status text property setter.
     *
     * @param statusText Status text
     */
    public void setStatusText(String statusText);


    /**
     * Status text property getter.
     *
     * @return String status text
     */
    public String getStatusText();


    /**
     * Path property setter.
     *
     * @param path Absolute path
     */
    public void setPath(String path);


    /**
     * Path property getter.
     *
     * @return String path
     */
    public String getPath();


    /**
     * Name property getter.
     *
     * @return String name
     */
    public String getName();


    /**
     * Set header.
     *
     * @param headerName Header name
     * @param headerValue Header value
     */
    public void setHeader(String headerName, String headerValue);


    /**
     * Get header.
     *
     * @param headerName Header name
     * @return String header value (null if the header doesn't exist)
     */
    public Header getHeader(String headerName);


    /**
     * Remove header.
     *
     * @param headerName Header name
     */
    public void removeHeader(String headerName);


    /**
     * Checks if this method's instance has already been used, and has not been
     * recycled.
     *
     * @return boolean True if the method's instance has already been used
     */
    public boolean hasBeenUsed();


    /**
     * True if this methods should automatically follow redirects.
     *
     * @return boolean True if auto redirect should be used for this method
     */
    public boolean followRedirects();

    /**
     * Set whether or not I should automatically follow redirects.
     *
     * @param followRedirects <tt>true</tt> if auto redirect should be used for this method
     */
    public void setFollowRedirects(boolean followRedirects);

    /**
     * Set the state token.
     */
    public void setState(State state);


    /**
     * Set URL parameter.
     *
     * @param parameterName Parameter name
     * @param parameterValue Parameter value
     */
    public void setParameter(String parameterName, String parameterValue);


    /**
     * Set query string.
     *
     * @param queryString Query string
     */
    public void setQueryString(String queryString);


    // ------------------------------------------------------ Interface Methods


    /**
     * Ensures the correctness of the request according to criterions which are
     * method dependent.
     *
     * @return boolean True if the method is valid
     */
    public boolean validate();


    /**
     * Recycle the method object, so that it can be reused again. Any attempt
     * to reuse an object without recycling it will throw a HttpException.
     */
    public void recycle();


    /**
     * Get headers.
     *
     * @return Enumeration
     */
    public Enumeration getHeaders();


    /**
     * Generate additional headers needed by the request.
     *
     * @param state State token
     */
    public void generateHeaders(String host, State state);


    /**
     * Is the query body submitted through an InputStream of with a String.
     * If an InputStream is available, it's used.
     *
     * @return boolean True if the content is avalable in an InputStream
     */
    public boolean isStreamedQuery();


    /**
     * Generate the query.
     *
     * @return String query
     */
    public String generateQuery();


    /**
     * Set the query String as a XML document.
     * If the query string is already given as an XML document
     * this low level function will set the query string directly.
     * This method make only sense in the context of lock, propFind,
     * and propPatch. In all other cases this method call is ignored.
     */
    public void setQuery(String query);


    /**
     * Stream the body of the query. This function should be used to send large
     * request bodies.
     */
    public void streamQuery(OutputStream out)
        throws IOException ;


    /**
     * Process response headers. The contract of this method is that it only
     * parses the response headers.
     *
     * @param headers Headers list
     */
    public void processResponseHeaders(Hashtable headers);


    /**
     * Parse response.
     *
     * @param is Input stream
     */
    public void parseResponse(InputStream is)
        throws IOException, HttpException;


    /**
     * Generate the HTTP request line.
     *
     * @return String request line
     */
    public String generateRequestLine();


    /**
     * Generate the HTTP request line, using a proxy server.
     *
     * @return String request line
     */
    public String generateRequestLine(String phost, int pport);


    /**
     * Return true if the method needs a content-length header in the request.
     *
     * @return true if a content-length header will be expected by the server
     */
    public boolean needContentLength();


    /**
     * Return true if the method's response is expected to have a body.
     * 
     * @return true if a response body should be expected by the client
     */
    public boolean hasResponseBody();


    /**
     * Return true if the method should ask for an expectation.
     *
     * @return true if an expectation will be sent
     */
    public boolean needExpectation();


}
