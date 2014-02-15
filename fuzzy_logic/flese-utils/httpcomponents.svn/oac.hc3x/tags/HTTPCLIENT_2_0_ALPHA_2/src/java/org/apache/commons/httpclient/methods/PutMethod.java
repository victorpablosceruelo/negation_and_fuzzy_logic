/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/PutMethod.java,v 1.19 2003/01/23 22:48:09 jsdever Exp $
 * $Revision: 1.19 $
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

import org.apache.commons.httpclient.HttpConstants;
import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethodBase;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;


/**
 * PUT Method.
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 *
 * @since 1.0
 */
public class PutMethod extends HttpMethodBase {


    // ----------------------------------------------------------- Constructors


    /**
     * No-arg constructor.
     *
     * @since 1.0
     */
    public PutMethod() {
        setFollowRedirects(false);
    }


    /**
     * Constructor specifying a URI.
     *
     * @param uri either an absolute or relative URI
     *
     * @since 1.0
     */
    public PutMethod(String uri) {
        super(uri);
        setFollowRedirects(false);
    }


    // ------------------------------------------------------- Instance Methods


    /**
     * Request body content to be sent.
     */
    private byte[] data = null;


    /**
    * Request body content to be sent.
     */
    private File file = null;


    /**
     * Request body content to be sent.
     */
    private URL url = null;


    // --------------------------------------------------------- Public Methods

    /**
     * Return <tt>"PUT"</tt>.
     * @return <tt>"PUT"</tt>
     *
     * @since 2.0
     */
    public String getName() {
        return "PUT";
    }

    /**
     * Set my request body content to the contents of a file.
     *
     * @since 2.0
     */
    public void setRequestBody(File file) throws IOException {
        checkNotUsed();
        this.file = file;
    }

    /**
     * Set my request body content to the resource at the specified URL.
     *
     * @since 2.0
     */
    public void setRequestBody(URL url) throws IOException {
        checkNotUsed();
        this.url = url;
    }

    /**
     * Set my request body content to the contents of a byte array.
     *
     * @since 2.0
     */
    public void setRequestBody(byte[] bodydata) {
        checkNotUsed();
        this.data = bodydata;
    }

    /**
     * Set my request body content to the contents of a string.
     *
     * @since 2.0
     */
    public void setRequestBody(String bodydata) {
        checkNotUsed();
        setRequestBody(HttpConstants.getContentBytes(bodydata, getRequestCharSet()));
    }

    /**
     * Set my request body content to the contents of an input stream.
     * The contents will be buffered into
     * memory. To upload large entities, it is recommended to first buffer the
     * data into a temporary file, and then send that file.
     *
     * @since 2.0
     */
    public void setRequestBody(InputStream is)
    throws IOException {
        log.trace("enter PutMethod.setRequestBody(InputStream)");

        checkNotUsed();
        byte[] buffer = new byte[4096];
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        int nb = 0;
        while (true) {
            nb = is.read(buffer);
            if (nb == -1) {
                break;
            }
            os.write(buffer, 0, nb);
        }
        data = os.toByteArray();
    }


    // ------------------------------------------------- HttpMethodBase Methods

    /**
     * Override the method of {@link HttpMethodBase}
     * to set the <tt>Expect</tt> header if it has
     * not already been set, in addition to the "standard"
     * set of headers.
     *
     * @since 2.0
     */
    protected void addRequestHeaders(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter PutMethod.addRequestHeaders(HttpState, HttpConnection)");

        super.addRequestHeaders(state,conn);
        // Send expectation header
        if(isHttp11() && null == getRequestHeader("expect")) {
            setRequestHeader("Expect","100-continue");
        }
    }

    /**
     * Override the method of {@link HttpMethodBase}
     * to not send any data until
     * the <tt>100 Continue</tt> status has not be
     * read.
     *
     * @since 2.0
     */
    protected boolean writeRequestBody(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter PutMethod.writeRequestBody(HttpState, HttpConnection)");
         if (getStatusLine() == null) {
             return false;
         }
         if(null != getRequestHeader("expect") &&
                 getStatusLine().getStatusCode() != HttpStatus.SC_CONTINUE) {
            return false;
        }
        OutputStream out = conn.getRequestOutputStream((isHttp11() && (null == getRequestHeader("Content-Length"))));

        InputStream inputStream = null;
        if (file != null && file.exists()) {
            inputStream = new FileInputStream(file);
        } else if (url != null) {
            inputStream = url.openConnection().getInputStream();
        } else if(data != null){
            inputStream = new ByteArrayInputStream(data);
        } else {
            return true;
        }

        byte[] buffer = new byte[4096];
        int nb = 0;
        while (true) {
            nb = inputStream.read(buffer);
            if (nb == -1) {
                break;
            }
            out.write(buffer, 0, nb);
        }
        out.flush();
        return true;
    }

    /**
     * Override the method of {@link HttpMethodBase}
     * to return the appropriate content length.
     *
     * @since 2.0
     */
    protected int getRequestContentLength() {
        log.trace("enter PutMethod.getRequestContentLength()");

        if(null != data) {
            return data.length;
        } else if(null != file && file.exists()) {
            return (int)(file.length());
        } else if(url != null) {
            return -1;
        } else {
            return 0;
        }
    }

    /**
     *
     * @since 1.0
     */
    public void recycle() {
        super.recycle();
        data = null;
        url = null;
        file = null;
    }

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(PutMethod.class);
}
