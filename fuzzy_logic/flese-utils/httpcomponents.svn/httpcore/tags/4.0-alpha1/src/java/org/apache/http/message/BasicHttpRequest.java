/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha1/src/java/org/apache/http/message/BasicHttpRequest.java $
 * $Revision: 385789 $
 * $Date: 2006-03-14 12:24:05 +0100 (Tue, 14 Mar 2006) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2006 The Apache Software Foundation
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
 */

package org.apache.http.message;

import org.apache.http.HttpRequest;
import org.apache.http.HttpVersion;
import org.apache.http.RequestLine;
import org.apache.http.params.HttpProtocolParams;

/**
 * Basic implementation of an HTTP request that can be modified.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 385789 $
 * 
 * @since 4.0
 */
public class BasicHttpRequest extends AbstractHttpMessage implements HttpRequest {
    
    private final RequestLine requestline;
    private final String method;
    private final String uri;
        
    public BasicHttpRequest(final String method, final String uri) {
        super();
        if (method == null) {
            throw new IllegalArgumentException("Method name may not be null");
        }
        if (uri == null) {
            throw new IllegalArgumentException("Request URI may not be null");
        }
        this.method = method;
        this.uri = uri;
        this.requestline = null;
    }

    public BasicHttpRequest(final RequestLine requestline) {
        super();
        if (requestline == null) {
            throw new IllegalArgumentException("Request line may not be null");
        }
        this.requestline = requestline;
        this.method = requestline.getMethod();
        this.uri = requestline.getUri();
    }

    public HttpVersion getHttpVersion() {
        if (this.requestline != null) {
            return this.requestline.getHttpVersion();
        } else {
            return HttpProtocolParams.getVersion(getParams());
        }
    }
    
    public RequestLine getRequestLine() {
        if (this.requestline != null) {
            return this.requestline;
        } else {
            HttpVersion ver = HttpProtocolParams.getVersion(getParams());
            return new RequestLine(this.method, this.uri, ver);
        }
    }
    
}
