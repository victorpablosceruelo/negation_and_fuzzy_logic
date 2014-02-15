/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/Attic/GenericResponse.java,v 1.1.2.2 2003/11/24 20:41:11 oglueck Exp $
 * $Revision: 1.1.2.2 $
 * $Date: 2003-11-24 21:41:11 +0100 (Mon, 24 Nov 2003) $
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

package org.apache.commons.httpclient.server;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.apache.commons.httpclient.Header;

/**
 * A generic HTTP response.
 * 
 * @author Christian Kohlschuetter
 */
public class GenericResponse implements HttpRequestHandler {
    private ByteArrayOutputStream bos = new ByteArrayOutputStream();
    private String statusLine, contentType;
    private String bodyString;
    private byte[] bodyBytes;
    private Header[] responseHeaders;

    public GenericResponse() throws IOException {
        this("HTTP/1.0 200 OK", "text/plain");
    }
    public GenericResponse(String statusLine, String contentType) {
        this(statusLine, contentType, (Header[])null);
    }

    public GenericResponse(
        String statusLine,
        String contentType,
        Header[] headers) {

        this(statusLine, (String) null, contentType, headers);
    }

    public GenericResponse(
        String statusLine,
        String bodyString,
        String contentType) {

        this(statusLine, bodyString, contentType, null);
    }

    public GenericResponse(
        String statusLine,
        String bodyString,
        String contentType,
        Header[] headers) {

        setStatusLine(statusLine);
        setContentType(contentType);
        setBodyString(bodyString);
        setupBody();
    }
    public GenericResponse(
        String statusLine,
        byte[] bodyBytes,
        String contentType,
        Header[] headers) {
        setStatusLine(statusLine);
        setContentType(contentType);
        setBodyBytes(bodyBytes);
        setupBody();
    }

    public String getContentType() {
        return contentType;
    }
    public void setContentType(String string) {
        this.contentType = string;
    }

    public void setBodyString(String string) {
        bodyString = string;
        bodyBytes = null;
    }
    public void setBodyBytes(byte[] body) {
        bodyBytes = body;
        bodyString = null;
    }

    public String getStatusLine() {
        return statusLine;
    }

    public void setStatusLine(String string) {
        statusLine = string;
    }

    public Header[] getResponseHeaders() {
        return responseHeaders;
    }
    public void setResponseHeaders(Header[] headers) {
        responseHeaders = headers;
    }

    public void setupBody() {
        try {
            if (bodyString != null) {
                ResponseWriter body = new ResponseWriter(bos);

                if (bodyString != null) {
                    body.print(bodyString);
                } else if (bodyBytes != null) {
                    body.write(bodyBytes);
                }

                body.close();
            }
        } catch (IOException e) {
            e.printStackTrace(System.err);
        }
    }

    public boolean processRequest(SimpleHttpServerConnection conn)  throws IOException {

        boolean haveContentLength = false;
        boolean haveContentType = false;
        ResponseWriter out = conn.getWriter();
        out.println(getStatusLine());
        if (responseHeaders != null) {
            for (int i = 0; i < responseHeaders.length; i++) {
                Header h = responseHeaders[i];
                String name = h.getName();
                if (name.equals("Content-Type")) {
                    haveContentType = true;
                } else if (name.equals("Content-Length")) {
                    haveContentLength = true;
                }

                String value = h.getValue();
                out.println(
                    ((null == name ? "" : name)
                        + ": "
                        + (null == value ? "" : value)));
            }
        }
        if (!haveContentLength) {
            out.print("Content-Length: ");
            out.println(bos.size());
        }
        if (!haveContentType && getContentType() != null) {
            out.print("Content-Type: ");
            out.print(getContentType());
            if (out.getEncoding() != null) {
                out.print("; charset=");
                out.println(out.getEncoding());
            }
        }
        out.println();
        out.write(bos.toByteArray());

        bos.close();
        return true;
    }
}
