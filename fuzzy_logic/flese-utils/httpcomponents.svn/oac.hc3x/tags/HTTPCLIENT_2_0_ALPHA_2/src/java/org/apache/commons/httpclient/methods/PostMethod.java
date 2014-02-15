/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/PostMethod.java,v 1.33 2003/01/23 22:48:08 jsdever Exp $
 * $Revision: 1.33 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
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
package org.apache.commons.httpclient.methods;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import org.apache.commons.httpclient.HttpConstants;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.ChunkedOutputStream;
import org.apache.commons.httpclient.ContentLengthInputStream;
import org.apache.commons.httpclient.util.URIUtil;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * POST Method. This class encapsulates the HTTP POST specification.  According
 * to <a href="http://www.ietf.org/rfc/rfc2616.txt">RFC2616</a>:
 * <blockquote>
 * The POST method is used to request that the origin server accept the entity
 * enclosed in the request as a new subordinate of the resource identified by
 * the Request-URI in the Request-Line. POST is designed to allow a uniform
 * method to cover the following functions:
 *
 * <ul>
 * <li>
 * Annotation of existing resources;
 * </li>
 * <li>
 * Posting a message to a bulletin board, newsgroup, mailing list, or similar
 * group of articles;
 * </li>
 * <li>
 * Providing a block of data, such as the result of submitting a form, to a
 * data-handling process;
 * </li>
 * <li>
 * Extending a database through an append operation.
 * </li>
 * </ul>
 *
 * </blockquote>
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author <a href="mailto:dsale@us.britannica.com">Doug Sale</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author Ortwin Glück
 * @since 1.0
 */
public class PostMethod extends GetMethod {
    //~ Static variables/initializers ··········································

    /**
     * The content length will be calculated automatically. This implies
     * buffering of the content.
     */
    public static final int CONTENT_LENGTH_AUTO = -2;

    /**
     * The request will use chunked transfer encoding. Content length is not
     * calculated and the content is not buffered.<br>
     */
    public static final int CONTENT_LENGTH_CHUNKED = -1;

    // -------------------------------------------------------------- Constants

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(PostMethod.class);

    /** The Content-Type header for www-form-urlcoded. */
    static final Header CONTENT_TYPE = new Header("Content-Type",
        "application/x-www-form-urlencoded");

    /** The buffered request body. */
    protected ByteArrayOutputStream buffer = null;

    /** The unbuffered request body. */
    protected InputStream requestBody = null;

    /** The buffered request body consisting of <code>NameValuePair</code>s */
    protected Vector parameters = new Vector();

    /** Counts how often the request was sent to the server. */
    protected int repeatCount = 0;

    /** The content length of the <code>requestBody</code> or one of
     *  <code>CONTENT_LENGTH_AUTO</code> and <code>CONTENT_LENGTH_CHUNKED</code>.
     */
    protected int requestContentLength = CONTENT_LENGTH_AUTO;

    //~ Constructors ···························································

    /**
     * No-arg constructor.
     *
     * @since 1.0
     */
    public PostMethod() {
        super();
        setFollowRedirects(false);
    }

    /**
     * Constructor specifying a URI.
     *
     * @param uri either an absolute or relative URI
     *
     * @since 1.0
     */
    public PostMethod(String uri) {
        super(uri);
        setFollowRedirects(false);
    }

    /**
     * Constructor specifying a URI and a tempDir.
     *
     * @param uri either an absolute or relative URI
     * @param tempDir directory to store temp files in
     *
     * @since 1.0
     */
    public PostMethod(String uti, String tempDir) {
        super(uti, tempDir);
        setFollowRedirects(false);
    }

    /**
     * Constructor specifying a URI, tempDir and tempFile.
     *
     * @param uri either an absolute or relative URI
     * @param tempDir directory to store temp files in
     * @param tempFile file to store temporary data in
     *
     * @since 1.0
     */
    public PostMethod(String uri, String tempDir, String tempFile) {
        super(uri, tempDir, tempFile);
        setFollowRedirects(false);
    }

    // ----------------------------------------------------------- Constructors

    //~ Methods ································································

    /**
     * A POST request can only be redirected if input is buffered. Overrides
     * method of {@link org.apache.commons.httpclient.HttpMethodBase}.
     *
     * @return true if request is buffered and <code>setFollowRedirects</code>
     *         was set to <code>true</code>.
     *
     * @since 2.0
     */
    public boolean getFollowRedirects() {
        if (!super.getFollowRedirects()) {
            return false;
        }

        return (buffer != null);
    }

    // ----------------------------------------------------- Instance Methods

    /**
     * Returns <tt>"POST"</tt>.
     *
     * @return <tt>"POST"</tt>
     *
     * @since 2.0
     */
    public String getName() {
        return "POST";
    }

    /**
     * Set the value of parameter with parameterName to parameterValue. Does
     * not preserve the initial insertion order.
     *
     * @param parameterName DOCUMENT ME!
     * @param parameterValue DOCUMENT ME!
     *
     * @throws IllegalStateException if my request body has already been
     *         generated.
     *
     * @since 2.0
     * @deprecated use {@link #removeParameter(String,String)} followed by
     *             {@link #addParameter(String,String)}.
     */
    public void setParameter(String parameterName, String parameterValue) {
        log.trace("enter PostMethod.setParameter(String, String)");

        if (null != requestBody) {
            throw new IllegalStateException("Request body already generated.");
        }

        removeParameter(parameterName, parameterValue);
        addParameter(parameterName, parameterValue);
    }

    /**
     * Gets the parameter of the specified name. If there exists more than one
     * parameter with the name paramName, then only the first one is returned.
     *
     * @param paramName DOCUMENT ME!
     *
     * @return If a parameter exists with the name argument, the coresponding
     *         NameValuePair is returned.  Otherwise null.
     *
     * @since 2.0
     */
    public NameValuePair getParameter(String paramName) {
        log.trace("enter PostMethod.getParameter(String)");

        if (paramName == null) {
            return null;
        }

        Iterator iter = parameters.iterator();

        while (iter.hasNext()) {
            NameValuePair parameter = (NameValuePair) iter.next();

            if (paramName.equals(parameter.getName())) {
                return parameter;
            }
        }

        return null;
    }

    /**
     * Gets the parameters currently added to the PostMethod. If there are no
     * parameters, a valid array is returned with zero elements. The returned
     * array object contains an array of pointers to  the internal data
     * members. TODO: is it ok to return internal data?
     *
     * @return An array of the current parameters
     *
     * @since 2.0
     * @see #getParameter(java.lang.String)
     */
    public NameValuePair[] getParameters() {
        log.trace("enter PostMethod.getParameters()");

        int numPairs = parameters.size();
        Object[] objectArr = parameters.toArray();
        NameValuePair[] nvPairArr = new NameValuePair[numPairs];

        for (int i = 0; i < numPairs; i++) {
            nvPairArr[i] = (NameValuePair) objectArr[i];
        }

        return nvPairArr;
    }

    /**
     * Sets the request body to be the specified string.
     *
     * <p>
     * Once this method has been invoked,  the request parameters  cannot be
     * altered until I am {@link #recycle recycled}.
     * </p>
     *
     * @param body Request content as a string
     *
     * @throws IllegalStateException if request params have been added
     *
     * @since 2.0
     */
    public void setRequestBody(String body) {
        log.trace("enter PostMethod.setRequestBody(String)");

        if (!parameters.isEmpty()) {
            throw new IllegalStateException(
                "Request parameters have already been added.");
        }

        if (body == null) {
            this.requestBody = null;
            return;
        }

        this.requestBody = new ByteArrayInputStream(
          HttpConstants.getContentBytes(body, getRequestCharSet()));
    }

    /**
     * Sets the request body to be the specified inputstream.
     *
     * <p>
     * Once this method has been invoked,  the request parameters  cannot be
     * altered until I am {@link #recycle recycled}.
     * </p>
     *
     * @param body DOCUMENT ME!
     *
     * @throws IllegalStateException if request params have been added
     *
     * @since 2.0
     */
    public void setRequestBody(InputStream body) {
        log.trace("enter PostMethod.getRequestBody(InputStream)");

        if (!parameters.isEmpty()) {
            throw new IllegalStateException(
                "Request parameters have already been added.");
        }

        this.requestBody = body;
    }

    /**
     * Gets the requestBody as it would be if it was executed.
     *
     * @return The request body if it has been set.  The generated  request
     *         body from the paramters if they exist.  Null otherwise.
     *
     * @since 2.0
     */
    public InputStream getRequestBody() {
        log.trace("enter PostMethod.getRequestBody()");

        if (requestBody != null) {
            return requestBody;
        } else if (!parameters.isEmpty()) {
            return generateRequestBody(parameters);
        } else {
            return null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return the request body as a string
     * @throws IOException DOCUMENT ME!
     *
     * @since 2.0
     */
    public String getRequestBodyAsString() throws IOException {
        log.trace("enter PostMethod.getRequestBodyAsString()");

        StringBuffer buffer = new StringBuffer();
        InputStream requestBody = getRequestBody();
        int data = requestBody.read();

        while (data != -1) {
            buffer.append((char) data);
            data = requestBody.read();
        }

        return buffer.toString();
    }

    /**
     * Sets length information about the request body.
     *
     * <p>
     * Note: If you specify a content length the request is unbuffered. This
     * prevents redirection and automatic retry if a request fails the first
     * time. This means that the HttpClient can not perform authorization
     * automatically but will throw an Exception. You will have to set the
     * necessary 'Authorization' or 'Proxy-Authorization' headers manually.
     * </p>
     *
     * @param length size in bytes or any of CONTENT_LENGTH_AUTO,
     *        CONTENT_LENGTH_CHUNKED. If number of bytes or CONTENT_LENGTH_CHUNKED
     *        is specified the content will not be buffered internally and the
     *        Content-Length header of the request will be used. In this case
     *        the user is responsible to supply the correct content length.
     *        If CONTENT_LENGTH_AUTO is specified the request will be buffered
     *        before it is sent over the network.
     * @throws RuntimeException if chunked transfer encoding is requested for
     *         a HTTP 1.0 request
     *
     * @since 2.0
     */
    public void setRequestContentLength(int length) {
        log.trace("enter PostMethod.setRequestContentLength(int)");

        if ((length == CONTENT_LENGTH_CHUNKED) && !isHttp11()) {
            throw new RuntimeException(
                "Chunked transfer encoding not allowed for HTTP/1.0");
        }

        requestContentLength = length;
    }

    /**
     * Add a new parameter to be used in the POST request body.
     *
     * @param paramName The parameter name to add.
     * @param paramValue The parameter value to add.
     *
     * @throws IllegalStateException if my request body has already been
     *         generated.
     * @throws IllegalArgumentException if either argument is null
     *
     * @since 1.0
     */
    public void addParameter(String paramName, String paramValue) {
        log.trace("enter PostMethod.addParameter(String, String)");

        if (null != requestBody) {
            throw new IllegalStateException("Request body already generated.");
        }

        if ((paramName == null) || (paramValue == null)) {
            throw new IllegalArgumentException(
                "Arguments to addParameter(String, String) cannot be null");
        } else {
            parameters.add(new NameValuePair(paramName, paramValue));
        }
    }

    /**
     * Add a new parameter to be used in the POST request body.
     *
     * @param param The parameter to add.
     *
     * @throws IllegalStateException if my request body has already been
     *         generated.
     * @throws IllegalArgumentException if the argument is null or contains
     *         null values
     *
     * @since 2.0
     * @see #addParameter(String,String)
     */
    public void addParameter(NameValuePair param) {
        log.trace("enter PostMethod.addParameter(NameValuePair)");

        if (null != requestBody) {
            throw new IllegalStateException("Request body already generated.");
        }

        if (null == param) {
            throw new IllegalArgumentException(
                "Argument to addParameter(NameValuePair) cannot be null");
        } else {
            addParameter(param.getName(), param.getValue());
        }
    }

    /**
     * Add an Array of parameters to be used in the POST request body. Logs a
     * warning if the parameters argument is null.
     *
     * @param parameters The array of parameters to add.
     *
     * @throws IllegalStateException if my request body has already been
     *         generated.
     *
     * @since 2.0
     * @see #addParameter(org.apache.commons.httpclient.NameValuePair)
     */
    public void addParameters(NameValuePair[] parameters) {
        log.trace("enter PostMethod.addParameters(NameValuePair[])");

        if (null != requestBody) {
            throw new IllegalStateException("Request body already generated.");
        }

        if (null == parameters) {
            log.warn("Attempt to addParameters(null) ignored");
        } else {
            for (int i = 0; i < parameters.length; i++) {
                addParameter(parameters[i]);
            }
        }
    }

    /**
     * Override method of {@link org.apache.commons.httpclient.HttpMethodBase}
     * to clear my request body.
     *
     * @since 1.0
     */
    public void recycle() {
        log.trace("enter PostMethod.recycle()");
        super.recycle();
        requestBody = null;
        requestContentLength = CONTENT_LENGTH_AUTO;
        buffer = null;
        repeatCount = 0;
        parameters.clear();
    }

    /**
     * Removes all parameters with the given paramName. If there is more than
     * one parameter with the given paramName, all of them are removed.  If
     * there is just one, it is removed.  If there are none, then the request
     * is ignored.
     *
     * @param paramName The parameter name to remove.
     *
     * @return true if at least one parameter was removed
     *
     * @throws IllegalStateException if my request body has already been
     *         generated.
     * @throws IllegalArgumentException When the parameter name passed is null
     *
     * @since 2.0
     */
    public boolean removeParameter(String paramName) {
        log.trace("enter PostMethod.removeParameter(String)");

        if (null != requestBody) {
            throw new IllegalStateException("Request body already generated.");
        }

        if (paramName == null) {
            throw new IllegalArgumentException(
                "Argument passed to removeParameter(String) cannot be null");
        }

        boolean removed = true;
        Iterator iter = parameters.iterator();

        while (iter.hasNext()) {
            NameValuePair pair = (NameValuePair) iter.next();

            if (paramName.equals(pair.getName())) {
                iter.remove();
                removed = true;
            }
        }

        return removed;
    }

    /**
     * Removes all parameter with the given paramName and paramValue. If there
     * is more than one parameter with the given paramName, only one is
     * removed.  If there are none, then the request is ignored.
     *
     * @param paramName The parameter name to remove.
     * @param paramValue The parameter value to remove.
     *
     * @return true if a parameter was removed.
     *
     * @throws IllegalStateException if my request body has already been
     *         generated.
     * @throws IllegalArgumentException when param name or value are null
     *
     * @since 2.0
     */
    public boolean removeParameter(String paramName, String paramValue) {
        log.trace("enter PostMethod.removeParameter(String, String)");

        if (null != requestBody) {
            throw new IllegalStateException("Request body already generated.");
        }

        if ((paramName == null) || (paramValue == null)) {
            throw new IllegalArgumentException(
                "Argument passed to removeParameter(String,String) cannot be "
                + "null");
        }

        Iterator iter = parameters.iterator();

        while (iter.hasNext()) {
            NameValuePair pair = (NameValuePair) iter.next();

            if (paramName.equals(pair.getName())
                && paramValue.equals(pair.getValue())) {
                iter.remove();

                return true;
            }
        }

        return false;
    }

    /**
     * Override method of {@link org.apache.commons.httpclient.HttpMethodBase}
     * to return the length of the request body.
     *
     * @return number of bytes in the request body
     *
     * @since 2.0
     */
    protected int getRequestContentLength() {
        log.trace("enter PostMethod.getRequestContentLength()");

        if (null == requestBody) {
            requestBody = generateRequestBody(parameters);
            bufferContent();
        }

        if (requestContentLength != CONTENT_LENGTH_AUTO) {
            return requestContentLength;
        }

        bufferContent();

        return requestContentLength;
    }

    /**
     * Override method of {@link org.apache.commons.httpclient.HttpMethodBase}
     * to  also add <tt>Content-Type</tt> header when appropriate.
     *
     * @param state DOCUMENT ME!
     * @param conn DOCUMENT ME!
     * @throws IOException DOCUMENT ME!
     * @throws HttpException DOCUMENT ME!
     *
     * @since 2.0
     */
    protected void addRequestHeaders(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        super.addRequestHeaders(state, conn);

        if (!parameters.isEmpty()) {
            //there are some parameters, so set the contentType header
            setRequestHeader(CONTENT_TYPE);
        }
    }

    /**
     * Override method of {@link org.apache.commons.httpclient.HttpMethodBase}
     * to write request parameters as the request body.  The input stream will
     * be truncated after the specified content length.
     *
     * @param state DOCUMENT ME!
     * @param conn DOCUMENT ME!
     *
     * @return always returns true
     *
     * @throws IOException if the stream ends before the specified content
     *         length. <p>
     * @throws HttpException DOCUMENT ME!
     *
     * @since 2.0
     */
    protected boolean writeRequestBody(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace(
            "enter PostMethod.writeRequestBody(HttpState, HttpConnection)");

        if (null == requestBody) {
            requestBody = generateRequestBody(parameters);
        }

        if ((repeatCount > 0) && (buffer == null)) {
            throw new HttpException(
                "Sorry, unbuffered POST request can not be repeated.");
        }

        repeatCount++;

        InputStream instream = this.requestBody;
        OutputStream outstream = conn.getRequestOutputStream();

        if (this.requestContentLength == CONTENT_LENGTH_CHUNKED) {
            outstream = new ChunkedOutputStream(outstream);
        }
        if (this.requestContentLength >= 0) {
            // don't need a watcher here - we're reading from something local,
            // not server-side.
            instream = new ContentLengthInputStream(instream, this.requestContentLength);
        }

        byte[] tmp = new byte[4096];
        int total = 0;
        int i = 0;
        while ((i = instream.read(tmp)) >= 0) {
            outstream.write(tmp, 0, i);
            total += i;
        }
        if (outstream instanceof ChunkedOutputStream) {
            ((ChunkedOutputStream)outstream).writeClosingChunk();
        }
        if ((this.requestContentLength > 0) && (total < this.requestContentLength)) {
            throw new IOException("Unexpected end of input stream after "
                +total +" bytes (expected "+ this.requestContentLength +" bytes)");
        }

        if (buffer != null) {
            //restore buffered content for repeated requests
            requestBody = new ByteArrayInputStream(buffer.toByteArray());
        }

        return true;
    }


    /**
     * Encode the list of parameters into a query stream.
     *
     * @param params the list of query name and value
     *
     * @return the query stream
     *
     * @since 1.0
     */
    protected InputStream generateRequestBody(List params) {
        log.trace("enter PostMethod.generateRequestBody(List)");
		String body = generateRequestBodyAsString(params);

        return new ByteArrayInputStream(
          HttpConstants.getContentBytes(body, getRequestCharSet()));
    }


    // ------------------------------------------------------------Class Methods

    /**
     * Encode the list of parameters into a query string.
     *
     * @param params the list of query name and value
     *
     * @return the query string
     *
     * @since 2.0
     */
    protected static String generateRequestBodyAsString(List params) {
        log.trace("enter PostMethod.generateRequestBodyAsString(List)");

        Iterator it = params.iterator();
        StringBuffer buff = new StringBuffer();

        while (it.hasNext()) {
            NameValuePair parameter = (NameValuePair) it.next();

            String queryName = null;
            try {
                queryName = URIUtil.encodeWithinQuery(parameter.getName());
            } catch (URIException urie) {
                log.error("encoding error within query name", urie);
                queryName = parameter.getName();
            }
            buff.append(queryName).append("=");
            String queryValue = null;
            try {
                queryValue = URIUtil.encodeWithinQuery(parameter.getValue());
            } catch (URIException urie) {
                log.error("encoding error within query value", urie);
                queryValue = parameter.getValue();
            }
            buff.append(queryValue);
            if (it.hasNext()) {
                buff.append("&");
            }
        }
        return buff.toString();
    }

    /**
     * Buffers the request body and calculates the content length. If the
     * method was called earlier it returns immediately.
     *
     * @since 1.0
     */
    private void bufferContent() {
        log.trace("enter PostMethod.bufferContent()");

        if (buffer != null) {
            return;
        }

        try {
            buffer = new ByteArrayOutputStream();

            byte[] data = new byte[10000];
            int l = requestBody.read(data);
            int total = 0;

            while (l > 0) {
                buffer.write(data, 0, l);
                total += l;
                l = requestBody.read(data);
            }

            requestBody = new ByteArrayInputStream(buffer.toByteArray());
            requestContentLength = total;
        } catch (IOException e) {
            requestBody = null;
            requestContentLength = 0;
        }
    }
}
