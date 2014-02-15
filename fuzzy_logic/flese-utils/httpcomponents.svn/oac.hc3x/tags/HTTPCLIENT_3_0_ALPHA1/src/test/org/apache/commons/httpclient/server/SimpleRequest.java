/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/SimpleRequest.java,v 1.1 2004/02/27 19:06:19 olegk Exp $
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
import org.apache.commons.httpclient.HeaderElement;
import org.apache.commons.httpclient.HeaderGroup;
import org.apache.commons.httpclient.NameValuePair;

/**
 * A generic HTTP request.
 * 
 * @author Oleg Kalnichevski
 */
public class SimpleRequest {
    
    private RequestLine requestLine = null;
    private String contentType = "text/plain";
    private String charSet = null;
    private String bodyString = null;
    private HeaderGroup headers = new HeaderGroup();

    public SimpleRequest() {
        super();
    }

    public SimpleRequest(
        final RequestLine requestLine,
        final Header[] headers,
        final String bodyString)
    {
        super();
        if (requestLine == null) {
            throw new IllegalArgumentException("Request line may not be null");
        }
        this.requestLine = requestLine;
        if (headers != null) {
            this.headers.setHeaders(headers);
            Header content = this.headers.getFirstHeader("Content-Type");
            if (content != null) {
                HeaderElement values[] = content.getElements();
                if (values.length == 1) {
                    this.contentType = values[0].getName();
                    NameValuePair param = values[0].getParameterByName("charset");
                    if (param != null) {
                        this.charSet = param.getValue();
                    }
                }
            }
        }
        this.bodyString = bodyString;
        
        
    }

    public String getContentType() {
        return this.contentType;
    }
    
    public String getCharSet() {
        return this.charSet;
    }
    
    public String getBodyString() {
        return this.bodyString;
    }

    public RequestLine getRequestLine() {
        return this.requestLine;
    }

    public boolean containsHeader(final String name) {
        return this.headers.containsHeader(name);
    }

    public Header[] getHeaders() {
        return this.headers.getAllHeaders();
    }

    public Header getFirstHeader(final String s) {
        return this.headers.getFirstHeader(s);
    }

    public Iterator getHeaderIterator() {
        return this.headers.getIterator();
    }
}
