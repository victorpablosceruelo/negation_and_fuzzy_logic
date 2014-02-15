/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha2/src/java/org/apache/http/impl/DefaultHttpResponseFactory.java $
 * $Revision: 378645 $
 * $Date: 2006-02-17 23:34:54 +0100 (Fri, 17 Feb 2006) $
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

package org.apache.http.impl;

import org.apache.http.HttpResponse;
import org.apache.http.HttpResponseFactory;
import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.StatusLine;
import org.apache.http.message.BasicHttpResponse;

/**
 * Default implementation of a factory for creating response objects.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 378645 $
 * 
 * @since 4.0
 */
public class DefaultHttpResponseFactory implements HttpResponseFactory {
    
    public DefaultHttpResponseFactory() {
        super();
    }

    public HttpResponse newHttpResponse(final HttpVersion ver, final int status) {
        if (ver == null) {
            throw new IllegalArgumentException("HTTP version may not be null");
        }
        StatusLine statusline = new StatusLine(ver, status, HttpStatus.getStatusText(status)); 
        return new BasicHttpResponse(statusline); 
    }
    
    public HttpResponse newHttpResponse(final StatusLine statusline) {
        if (statusline == null) {
            throw new IllegalArgumentException("Status line may not be null");
        }
        return new BasicHttpResponse(statusline); 
    }
}
