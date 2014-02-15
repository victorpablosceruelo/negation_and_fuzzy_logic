/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta3/module-nio/src/main/java/org/apache/http/nio/NHttpConnection.java $
 * $Revision: 613298 $
 * $Date: 2008-01-18 23:09:22 +0100 (Fri, 18 Jan 2008) $
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

package org.apache.http.nio;

import org.apache.http.HttpConnection;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.protocol.HttpContext;

/**
 * Abstract non-blocking HTTP connection interface. It contains the current
 * HTTP context, as well as the actual HTTP request and HTTP response objects
 * that are being received / transferred over this connection.
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public interface NHttpConnection extends HttpConnection, IOControl {

    public static final int ACTIVE      = 0;
    public static final int CLOSING     = 1;
    public static final int CLOSED      = 2;
    
    int getStatus();
    
    /** 
     * Returns the current HTTP request if one is being received / transmitted.
     * Otherwise returns <tt>null</tt>.
     * 
     * @return an HTTP request if available. Otherwise returns <tt>null</tt>.
     */
    HttpRequest getHttpRequest();

    /** 
     * Returns the current HTTP response if one is being received / transmitted. 
     * Otherwise returns <tt>null</tt>.
     * 
     * @return an HTTP response if available. Otherwise returns <tt>null</tt>.
     */
    HttpResponse getHttpResponse();
    
    /**
     * Returns an HTTP execution context associated with this connection.
     * @return HTTP context
     */
    HttpContext getContext();
    
}
