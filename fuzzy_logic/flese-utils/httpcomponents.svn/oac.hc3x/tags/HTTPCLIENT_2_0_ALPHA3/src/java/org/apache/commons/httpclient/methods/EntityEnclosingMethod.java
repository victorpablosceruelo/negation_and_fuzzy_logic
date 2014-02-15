/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/EntityEnclosingMethod.java,v 1.9 2003/02/13 21:31:53 olegk Exp $
 * $Revision: 1.9 $
 * $Date: 2003-02-13 22:31:53 +0100 (Thu, 13 Feb 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2003 The Apache Software Foundation.  All rights
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

package org.apache.commons.httpclient.methods;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.ByteArrayInputStream;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.io.ByteArrayOutputStream;

import org.apache.commons.httpclient.HttpConstants;
import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.ChunkedOutputStream;
import org.apache.commons.httpclient.ContentLengthInputStream;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * This abstract class serves as a foundation for all HTTP methods 
 * that can enclose an entity within requests 
 *
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 *
 * @since 2.0beta1
 * @version $Revision: 1.9 $
 */
public abstract class EntityEnclosingMethod extends GetMethod {

    // ----------------------------------------- Static variables/initializers

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

    /** LOG object for this class. */
    private static final Log LOG = LogFactory.getLog(EntityEnclosingMethod.class);

    /** The buffered request body, if any. */
    private byte[] buffer = null;

    /** The unbuffered request body, if any. */
    private InputStream requestBodyStream = null;

    /** Counts how often the request was sent to the server. */
    private int repeatCount = 0;

    /** The content length of the <code>requestBodyStream</code> or one of
     *  <code>CONTENT_LENGTH_AUTO</code> and <code>CONTENT_LENGTH_CHUNKED</code>.
     */
    private int requestContentLength = CONTENT_LENGTH_AUTO;

    /** This flag specifies whether "expect: 100-continue" handshake is
     * to be used prior to sending the requesst body */
    private boolean useExpectHeader = true;
    
    // ----------------------------------------------------------- Constructors

    /**
     * No-arg constructor.
     *
     * @since 2.0
     */
    public EntityEnclosingMethod() {
        super();
        setFollowRedirects(false);
    }

    /**
     * Constructor specifying a URI.
     *
     * @param uri either an absolute or relative URI
     *
     * @since 2.0
     */
    public EntityEnclosingMethod(String uri) {
        super(uri);
        setFollowRedirects(false);
    }

    /**
     * Constructor specifying a URI and a tempDir.
     *
     * @param uri either an absolute or relative URI
     * @param tempDir directory to store temp files in
     *
     * @deprecated the client is responsible for disk I/O
     * @since 2.0
     */
    public EntityEnclosingMethod(String uri, String tempDir) {
        super(uri, tempDir);
        setFollowRedirects(false);
    }

    /**
     * Constructor specifying a URI, tempDir and tempFile.
     *
     * @param uri either an absolute or relative URI
     * @param tempDir directory to store temp files in
     * @param tempFile file to store temporary data in
     *
     * @deprecated the client is responsible for disk I/O
     * @since 2.0
     */
    public EntityEnclosingMethod(String uri, String tempDir, String tempFile) {
        super(uri, tempDir, tempFile);
        setFollowRedirects(false);
    }


    /**
     * Entity enclosing requests cannot be redirected without user intervention
     * according to RFC 2616.
     *
     * @return <code>false</code>.
     *
     * @since 2.0
     */
    public boolean getFollowRedirects() {
        return false;
    }


    /**
     * Entity enclosing requests cannot be redirected without user intervention 
     * according to RFC 2616.
     *
     * @param followRedirects must always be <code>false</code>
     */
    public void setFollowRedirects(boolean followRedirects) {
        if (followRedirects == true) {
            // TODO: EntityEnclosingMethod should inherit from HttpMethodBase rather than GetMethod
            // Enable exception once the inheritence is fixed
            //throw new IllegalArgumentException(
            //  "Entity enclosing requests cannot be redirected without user intervention");
        }
        super.setFollowRedirects(false);
    }


    /**
     * Returns the useExpectHeader.
     * @return boolean
     */
    public boolean getUseExpectHeader() {
        return this.useExpectHeader;
    }

    /**
     * Sets the useExpectHeader.
     * @param value The useExpectHeader to set
     */
    public void setUseExpectHeader(boolean value) {
        this.useExpectHeader = value;
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
     *
     */
    public void setRequestContentLength(int length) {
        LOG.trace("enter EntityEnclosingMethod.setRequestContentLength(int)");
        this.requestContentLength = length;
    }

    /**
     * Override method of {@link org.apache.commons.httpclient.HttpMethodBase}
     * to return the length of the request body.
     *
     * @return number of bytes in the request body
     */
    protected int getRequestContentLength() {
        LOG.trace("enter EntityEnclosingMethod.getRequestContentLength()");

        if (this.requestContentLength != CONTENT_LENGTH_AUTO) {
            return this.requestContentLength;
        }
        bufferContent();

        return (this.buffer == null) ? 0 : this.buffer.length;
    }

    /**
     * Sets the request body to be the specified inputstream.
     *
     * @param body Request body content as {@link java.io.InputStream}
     */
    public void setRequestBody(InputStream body) {
        LOG.trace("enter EntityEnclosingMethod.setRequestBody(InputStream)");
        this.requestBodyStream = body;
        this.buffer = null;
    }

    /**
     * Gets the request body as a stream.
     *
     * @return The request body {@link java.io.InputStream} if it has been set.
     */
    public InputStream getRequestBody() {
        LOG.trace("enter EntityEnclosingMethod.getRequestBody()");
        if (this.buffer != null) {
            return new ByteArrayInputStream(this.buffer); 
        } else {
            return this.requestBodyStream;
        }
    }

    /**
     * Sets the request body to be the specified string.
     *
     * @param body Request body content as a string
     */
    public void setRequestBody(String body) {
        LOG.trace("enter EntityEnclosingMethod.setRequestBody(String)");

        if (body == null) {
            this.requestBodyStream = null;
            this.buffer = null;
            return;
        }
        this.buffer = HttpConstants.getContentBytes(body, getRequestCharSet());
    }

    /**
     * Gets the request body as a String.
     *
     * @return the request body as a string
     * 
     * @throws IOException when i/o errors occur reading the request
     */
    public String getRequestBodyAsString() throws IOException {
        LOG.trace("enter EntityEnclosingMethod.getRequestBodyAsString()");

        Reader instream = null;
        try {
            instream = new InputStreamReader(getRequestBody(), getRequestCharSet());
        } catch (UnsupportedEncodingException e) {
            if (LOG.isWarnEnabled()) {
                LOG.warn("Unsupported encoding: " + e.getMessage());
            }
            instream = new InputStreamReader(getRequestBody());
        }

        StringBuffer buffer = new StringBuffer();
        char[] tmp = new char[4096];
        int l = 0;
        while ((l = instream.read(tmp)) >= 0) {
            buffer.append(tmp, 0, l);
        }

        return buffer.toString();
    }


    /**
     * Set the <tt>Expect</tt> header if it has not already been set, 
     * in addition to the "standard" set of headers.
     *
     * @param state the client state
     * @param conn the connection to write to
     *
     * @throws HttpException when a protocol error occurs or state is invalid
     * @throws IOException when i/o errors occur reading the response
     */
    protected void addRequestHeaders(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        LOG.trace("enter EntityEnclosingMethod.addRequestHeaders(HttpState, HttpConnection)");
        
        super.addRequestHeaders(state, conn);
        // If the request is being retried, the header may already be present
        boolean headerPresent = (getRequestHeader("Expect") != null);
        // See if the expect header should be sent
        // = HTTP/1.1 or higher
        // = request body present
        if (getUseExpectHeader() && isHttp11() 
                && ((this.requestBodyStream != null) || this.buffer != null)) {
            if (!headerPresent) {
                setRequestHeader("Expect", "100-continue");
            }
        } else {
            if (headerPresent) {
                removeRequestHeader("Expect");
            }
        }
    }

    /**
     * Override method of {@link org.apache.commons.httpclient.HttpMethodBase}
     * to write request parameters as the request body.  The input stream will
     * be truncated after the specified content length.
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
        LOG.trace(
            "enter EntityEnclosingMethod.writeRequestBody(HttpState, HttpConnection)");
        
        if (getRequestHeader("Expect") != null) {
            if (getStatusLine() == null) {
                LOG.debug("Expecting response");
                return false;
            }
            if (getStatusLine().getStatusCode() != HttpStatus.SC_CONTINUE) {
                LOG.debug("Expecting 100-continue");
                return false;
            }
        }

        int contentLength = getRequestContentLength();

        if ((contentLength == CONTENT_LENGTH_CHUNKED) && !isHttp11()) {
            throw new HttpException(
                "Chunked transfer encoding not allowed for HTTP/1.0");
        }
        InputStream instream = getRequestBody();
        if (instream == null) {
            LOG.debug("Request body is empty");
            return true;
        }

        if ((this.repeatCount > 0) && (this.buffer == null)) {
            throw new HttpException(
                "Unbuffered entity enclosing request can not be repeated.");
        }

        this.repeatCount++;

        OutputStream outstream = conn.getRequestOutputStream();
        if (contentLength == CONTENT_LENGTH_CHUNKED) {
            outstream = new ChunkedOutputStream(outstream);
        }
        if (contentLength >= 0) {
            // don't need a watcher here - we're reading from something local,
            // not server-side.
            instream = new ContentLengthInputStream(instream, contentLength);
        }

        byte[] tmp = new byte[4096];
        int total = 0;
        int i = 0;
        while ((i = instream.read(tmp)) >= 0) {
            outstream.write(tmp, 0, i);
            total += i;
        }
        // This is hardly the most elegant solution to closing chunked stream
        if (outstream instanceof ChunkedOutputStream) {
            ((ChunkedOutputStream) outstream).writeClosingChunk();
        }
        if ((contentLength > 0) && (total < contentLength)) {
            throw new IOException("Unexpected end of input stream after "
                + total + " bytes (expected " + contentLength + " bytes)");
        }
        LOG.debug("Request body sent");
        return true;
    }

    /**
     * Override method of {@link org.apache.commons.httpclient.HttpMethodBase}
     * to clear my request body.
     */
    public void recycle() {
        LOG.trace("enter EntityEnclosingMethod.recycle()");
        this.requestContentLength = CONTENT_LENGTH_AUTO;
        this.requestBodyStream = null;
        this.buffer = null;
        this.repeatCount = 0;
        super.recycle();
    }

    /**
     * Buffers the request body and calculates the content length. If the
     * method was called earlier it returns immediately.
     */
    protected void bufferContent() {
        LOG.trace("enter EntityEnclosingMethod.bufferContent()");

        if (this.buffer != null) {
            return;
        }
        if (this.requestBodyStream == null) {
            return;
        }
        try {
            ByteArrayOutputStream tmp = new ByteArrayOutputStream();
            byte[] data = new byte[4096];
            int l = 0;
            while ((l = this.requestBodyStream.read(data)) >= 0) {
                tmp.write(data, 0, l);
            }
            this.buffer = tmp.toByteArray();
            this.requestBodyStream = null;
        } catch (IOException e) {
            LOG.error(e.getMessage(), e);
            this.buffer = null;
            this.requestBodyStream = null;
        }
    }
}
