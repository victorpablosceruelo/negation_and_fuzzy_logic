/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpConstants.java,v 1.3 2003/01/23 22:47:46 jsdever Exp $
 * $Revision: 1.3 $
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
package org.apache.commons.httpclient;


import java.io.UnsupportedEncodingException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


/**
 * DOCUMENT ME!
 *
 * @author Oleg Kalnichevski
 */
public class HttpConstants {

    /** Character set used to encode HTTP protocol elements */
    public static final String HTTP_ELEMENT_CHARSET = "US-ASCII";

    /** Default content encoding chatset */
    protected static final String DEFAULT_CONTENT_CHARSET = "ISO-8859-1";

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(HttpConstants.class);

    /**
     * Converts the specified string to a byte array of HTTP element characters.
     * This method is to be used when encoding content of HTTP elements (such as request headers)
     *
     * @param data the string to be encoded
     */
    public static byte[] getBytes(final String data) {

        if (data == null) {
            throw new IllegalArgumentException("Parameter may not be null");
        }

        try {
            return data.getBytes(HTTP_ELEMENT_CHARSET);
        } catch (UnsupportedEncodingException e) {

            if (log.isWarnEnabled()) {
                log.warn("Unsupported encoding: " + HTTP_ELEMENT_CHARSET + ". System default encoding used");
            }

            return data.getBytes();
        }
    }

    /**
     * Converts the byte array of HTTP element characters to a string
     * This method is to be used when decoding content of HTTP elements (such as response headers)
     *
     * @param data the byte array to be encoded
     */
    public static String getString(final byte[] data) {

        if (data == null) {
            throw new IllegalArgumentException("Parameter may not be null");
        }

        try {
            return new String(data, HTTP_ELEMENT_CHARSET);
        } catch (UnsupportedEncodingException e) {

            if (log.isWarnEnabled()) {
                log.warn("Unsupported encoding: " + HTTP_ELEMENT_CHARSET + ". System default encoding used");
            }

            return new String(data);
        }
    }

    /**
     * Converts the specified string to a byte array of HTTP content charachetrs
     * This method is to be used when encoding content of HTTP request/response
     * If the specified charset is not supported, default HTTP content encoding
     * (ISO-8859-1) is applied
     *
     * @param data the string to be encoded
     * @param charset the desired character encoding
     */
    public static byte[] getContentBytes(final String data, String charset) {

        if (data == null) {
            throw new IllegalArgumentException("Parameter may not be null");
        }

        if ((charset == null) || (charset.equals(""))) {
            charset = DEFAULT_CONTENT_CHARSET;
        }

        try {
            return data.getBytes(charset);
        } catch (UnsupportedEncodingException e) {

            if (log.isWarnEnabled()) {
                log.warn("Unsupported encoding: " + charset + ". HTTP default encoding used");
            }

            try {
                return data.getBytes(DEFAULT_CONTENT_CHARSET);
            } catch (UnsupportedEncodingException e2) {

                if (log.isWarnEnabled()) {
                    log.warn("Unsupported encoding: " + DEFAULT_CONTENT_CHARSET + ". System encoding used");
                }

                return data.getBytes();
            }
        }
    }

    /**
     * Converts the byte array of HTTP content characters to a string
     * This method is to be used when decoding content of HTTP request/response
     * If the specified charset is not supported, default HTTP content encoding
     * (ISO-8859-1) is applied
     *
     * @param data the byte array to be encoded
     * @param charset the desired character encoding
     */
    public static String getContentString(final byte[] data, String charset) {

        if (data == null) {
            throw new IllegalArgumentException("Parameter may not be null");
        }

        if ((charset == null) || (charset.equals(""))) {
            charset = DEFAULT_CONTENT_CHARSET;
        }

        try {
            return new String(data, charset);
        } catch (UnsupportedEncodingException e) {

            if (log.isWarnEnabled()) {
                log.warn("Unsupported encoding: " + DEFAULT_CONTENT_CHARSET + ". Default HTTP encoding used");
            }

            try {
                return new String(data, DEFAULT_CONTENT_CHARSET);
            } catch (UnsupportedEncodingException e2) {

                if (log.isWarnEnabled()) {
                    log.warn("Unsupported encoding: " + DEFAULT_CONTENT_CHARSET + ". System encoding used");
                }

                return new String(data);
            }
        }
    }

    /**
     * Converts the specified string to a byte array of HTTP content charachetrs
     * using default HTTP content encoding (ISO-8859-1)
     * This method is to be used when encoding content of HTTP request/response
     *
     * @param data the string to be encoded
     */
    public static byte[] getContentBytes(final String data) {
        return getContentBytes(data, null);
    }

    /**
     * Converts the byte array of HTTP content characters to a string using default
     * HTTP content encoding (ISO-8859-1)
     * This method is to be used when decoding content of HTTP request/response
     *
     * @param data the byte array to be encoded
     */
    public static String getContentString(final byte[] data) {
        return getContentString(data, null);
    }

    /**
     * Converts the specified string to byte array of ASCII characters.
     *
     * @param data the string to be encoded
     */
    public static byte[] getAsciiBytes(final String data) {

        if (data == null) {
            throw new IllegalArgumentException("Parameter may not be null");
        }

        try {
            return data.getBytes("US-ASCII");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException("HTTP client requires ASCII support");
        }
    }

    /**
     * Converts the byte array of ASCII characters to a string
     * This method is to be used when decoding content of HTTP elements (such as response headers)
     *
     * @param data the byte array to be encoded
     */
    public static String getAsciiString(final byte[] data) {

        if (data == null) {
            throw new IllegalArgumentException("Parameter may not be null");
        }

        try {
            return new String(data, "US-ASCII");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException("HTTP client requires ASCII support");
        }
    }
}
