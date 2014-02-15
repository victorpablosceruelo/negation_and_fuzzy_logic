/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-main/src/main/java/org/apache/http/protocol/HttpRequestHandlerRegistry.java $
 * $Revision: 514424 $
 * $Date: 2007-03-04 17:07:04 +0100 (Sun, 04 Mar 2007) $
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

package org.apache.http.protocol;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Maintains a map of HTTP request handlers keyed by a request URI pattern. 
 * {@link HttpRequestHandler} instances can be looked up by request URI
 * using the {@link HttpRequestHandlerResolver} interface.<br/>
 * Patterns may have three formats:
 * <ul>
 *   <li><code>*</code></li>
 *   <li><code>*&lt;uri&gt;</code></li>
 *   <li><code>&lt;uri&gt;*</code></li>
 * </ul>
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 514424 $
 */
public class HttpRequestHandlerRegistry implements HttpRequestHandlerResolver {

    private final Map handlerMap;
    
    public HttpRequestHandlerRegistry() {
        super();
        this.handlerMap = new HashMap();
    }
    
    public void register(final String pattern, final HttpRequestHandler handler) {
        if (pattern == null) {
            throw new IllegalArgumentException("URI request pattern may not be null");
        }
        if (handler == null) {
            throw new IllegalArgumentException("HTTP request handelr may not be null");
        }
        this.handlerMap.put(pattern, handler);
    }
    
    public void unregister(final String pattern) {
        if (pattern == null) {
            return;
        }
        this.handlerMap.remove(pattern);
    }
    
    public void setHandlers(final Map map) {
        if (map == null) {
            throw new IllegalArgumentException("Map of handlers may not be null");
        }
        this.handlerMap.clear();
        this.handlerMap.putAll(map);
    }
    
    public HttpRequestHandler lookup(String requestURI) {
        if (requestURI == null) {
            throw new IllegalArgumentException("Request URI may not be null");
        }
        //Strip away the query part part if found
        int index = requestURI.indexOf("?");
        if (index != -1) {
            requestURI = requestURI.substring(0, index);
        }
        
        // direct match?
        Object handler = this.handlerMap.get(requestURI);
        if (handler == null) {
            // pattern match?
            String bestMatch = null;
            for (Iterator it = this.handlerMap.keySet().iterator(); it.hasNext();) {
                String pattern = (String) it.next();
                if (matchUriRequestPattern(pattern, requestURI)) {
                    // we have a match. is it any better?
                    if (bestMatch == null 
                            || (bestMatch.length() < pattern.length())
                            || (bestMatch.length() == pattern.length() && pattern.endsWith("*"))) {
                        handler = this.handlerMap.get(pattern);
                        bestMatch = pattern;
                    }
                }
            }
        }
        return (HttpRequestHandler) handler;
    }

    protected boolean matchUriRequestPattern(final String pattern, final String requestUri) {
        if (pattern.equals("*")) {
            return true;
        } else {
            return 
            (pattern.endsWith("*") && requestUri.startsWith(pattern.substring(0, pattern.length() - 1))) ||
            (pattern.startsWith("*") && requestUri.endsWith(pattern.substring(1, pattern.length())));
        }
    }
    
}
