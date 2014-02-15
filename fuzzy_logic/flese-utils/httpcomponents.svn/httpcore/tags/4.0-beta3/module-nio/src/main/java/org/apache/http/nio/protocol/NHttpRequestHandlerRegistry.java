/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta3/module-nio/src/main/java/org/apache/http/nio/protocol/NHttpRequestHandlerRegistry.java $
 * $Revision: 667632 $
 * $Date: 2008-06-13 21:56:56 +0200 (Fri, 13 Jun 2008) $
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

package org.apache.http.nio.protocol;

import java.util.Map;

import org.apache.http.protocol.UriPatternMatcher;

/**
 * Maintains a map of HTTP request handlers keyed by a request URI pattern.
 * {@link NHttpRequestHandler} instances can be looked up by request URI
 * using the {@link NHttpRequestHandlerResolver} interface.<br/>
 * Patterns may have three formats:
 * <ul>
 *   <li><code>*</code></li>
 *   <li><code>*&lt;uri&gt;</code></li>
 *   <li><code>&lt;uri&gt;*</code></li>
 * </ul>
 *
 * @version $Revision: 667632 $
 */
public class NHttpRequestHandlerRegistry implements NHttpRequestHandlerResolver {

    private final UriPatternMatcher matcher;

    public NHttpRequestHandlerRegistry() {
        matcher = new UriPatternMatcher();
    }

    public void register(final String pattern, final NHttpRequestHandler handler) {
        matcher.register(pattern, handler);
    }

    public void unregister(final String pattern) {
        matcher.unregister(pattern);
    }

    public void setHandlers(final Map<String, ? extends NHttpRequestHandler> map) {
        matcher.setHandlers(map);
    }

    public NHttpRequestHandler lookup(String requestURI) {
        return (NHttpRequestHandler) matcher.lookup(requestURI);
    }

}
