/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/Attic/GenericResponse.java,v 1.1.2.3 2004/02/22 18:21:18 olegk Exp $
 * $Revision: 1.1.2.3 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2004 The Apache Software Foundation
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
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
