/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/SimpleResponse.java,v 1.1 2004/02/27 19:04:32 olegk Exp $
 * $Revision: 1.1 $
 * $Date: 2004-02-27 20:06:19 +0100 (Fri, 27 Feb 2004) $
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

import java.util.Iterator;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HeaderGroup;

/**
 * A generic HTTP response.
 * 
 * @author Christian Kohlschuetter
 * @author Oleg Kalnichevski
 */
public class SimpleResponse {
    
    private String statusLine = "HTTP/1.0 200 OK";
    private String contentType = "text/plain";
    private String bodyString = null;
    private HeaderGroup headers = new HeaderGroup();

    public SimpleResponse() {
        super();
    }

    public SimpleResponse(final String statusLine) {
        super();
        this.statusLine = statusLine;
    }

    public String getContentType() {
        return this.contentType;
    }
    
    public void setContentType(String string) {
        this.contentType = string;
    }

    public void setBodyString(String string) {
        this.bodyString = string;
    }
    
    public String getBodyString() {
        return this.bodyString;
    }

    public String getStatusLine() {
        return this.statusLine;
    }

    public void setStatusLine(String string) {
        this.statusLine = string;
    }

    public boolean containsHeader(final String name) {
        return this.headers.containsHeader(name);
    }

    public Header[] getHeaders() {
        return this.headers.getAllHeaders();
    }

    public void setHeader(final Header header) {
        if (header == null) {
            return;
        }
        Header[] headers = this.headers.getHeaders(header.getName());
        for (int i = 0; i < headers.length; i++) {
            this.headers.removeHeader(headers[i]);
        }
        this.headers.addHeader(header);
    }

    public void addHeader(final Header header) {
        if (header == null) {
            return;
        }
        this.headers.addHeader(header);
    }

    public void setHeaders(final Header[] headers) {
        if (headers == null) {
            return;
        }
        this.headers.setHeaders(headers);
    }

    public Iterator getHeaderIterator() {
        return this.headers.getIterator();
    }
}
