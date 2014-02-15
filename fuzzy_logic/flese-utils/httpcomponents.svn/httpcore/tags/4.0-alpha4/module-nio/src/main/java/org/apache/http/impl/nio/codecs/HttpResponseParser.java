/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-nio/src/main/java/org/apache/http/impl/nio/codecs/HttpResponseParser.java $
 * $Revision: 505744 $
 * $Date: 2007-02-10 19:58:45 +0100 (Sat, 10 Feb 2007) $
 *
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.http.impl.nio.codecs;

import org.apache.http.HttpException;
import org.apache.http.HttpMessage;
import org.apache.http.HttpResponseFactory;
import org.apache.http.StatusLine;
import org.apache.http.message.BasicStatusLine;
import org.apache.http.impl.nio.reactor.SessionInputBuffer;
import org.apache.http.util.CharArrayBuffer;

public class HttpResponseParser extends HttpMessageParser {
    
    private final HttpResponseFactory responseFactory;
    
    public HttpResponseParser(
            final SessionInputBuffer buffer,
            final HttpResponseFactory responseFactory) {
        super(buffer);
        if (responseFactory == null) {
            throw new IllegalArgumentException("Response factory may not be null");
        }
        this.responseFactory = responseFactory;
    }

    protected HttpMessage createMessage(final CharArrayBuffer buffer) 
            throws HttpException {
        StatusLine statusline = BasicStatusLine.parse(buffer, 0, buffer.length());
        return this.responseFactory.newHttpResponse(statusline, null);
    }

}
