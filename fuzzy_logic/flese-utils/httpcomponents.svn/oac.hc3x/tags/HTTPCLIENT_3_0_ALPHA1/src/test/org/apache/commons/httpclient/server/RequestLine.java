/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/RequestLine.java,v 1.3 2004/02/22 18:08:52 olegk Exp $
 * $Revision: 1.3 $
 * $Date: 2004-02-22 19:08:52 +0100 (Sun, 22 Feb 2004) $
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

import java.util.NoSuchElementException;
import java.util.StringTokenizer;

/**
 * Defines a HTTP request-line, consisting of method name, URI and protocol.
 * Instances of this class are immutable.
 * 
 * @author Christian Kohlschuetter
 */
public class RequestLine {
    private String method, uri, protocol;

    public static RequestLine parseLine(String l) {
        String method = null;
        String uri = null;
        String protocol = null;
        try {
            StringTokenizer st = new StringTokenizer(l, " ");
            method = st.nextToken();
            uri = st.nextToken();
            protocol = st.nextToken();
        } catch (NoSuchElementException e) {
        }

        return new RequestLine(method, uri, protocol);
    }
    
    public RequestLine(String method, String uri, String protocol) {
        this.method = method;
        this.uri = uri;
        this.protocol = protocol;
    }

    public String getMethod() {
        return method;
    }

    public String getProtocol() {
        return protocol;
    }

    public String getUri() {
        return uri;
    }

    public String toString() {
        StringBuffer sb = new StringBuffer();
        if(method != null) {
            sb.append(method);
            if(uri != null) {
                sb.append(" ");
                sb.append(uri);
                if(protocol != null) {
                    sb.append(" ");
                    sb.append(protocol);
                }
            }
        }
        return sb.toString();
    }
}
