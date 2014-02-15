/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/HttpDispatcher.java $
 * $Revision: 489367 $
 * $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
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

package org.apache.http.async;


import java.io.IOException;

import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpException;
import org.apache.http.protocol.HttpContext;


/**
 * Asynchronous dispatcher for HTTP requests.
 * A dispatcher schedules HTTP requests for asynchronous execution.
 * It will generate a {@link HttpHandle handle} for each request,
 * so that applications can access the responses as they arrive.
 * <br/>
 * Dispatchers are thread safe. An application can instantiate one
 * dispatcher and use it from multiple threads.
 * <br/>
 * A dispatcher typically makes use of some background threads and
 * client {@link org.apache.http.HttpClientConnection connections}
 * for processing requests and responses.
 * It is important to understand that callbacks from the dispatcher
 * to the application are executed by shared background threads.
 * For proper operation of a dispatcher, applications MUST make sure
 * that callbacks return swiftly. Prolonged processing of responses
 * MUST be delegated from the callbacks to application threads.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public interface HttpDispatcher {


    /**
     * Send an HTTP request asynchronously.
     * The dispatcher assumes responsibility for the request, which
     * will eventually be executed. The returned handle can be used
     * to synchronize with the execution, and to access the response.
     * <p>
     * <b>Note:</b> The <code>target</code> argument is preliminary, until
     * we've figured out a way to specify target host and an optional proxy.
     * </p>
     *
     * @param req         the request to dispatch.
     *                    It must not be modified while the responsibility
     *                    remains with the dispatcher.
     * @param target      the host to which the request should be sent
     * @param ctxt        the context for dispatching this request, or
     *                    <code>null</code> to use the
     *                    {@link #getDefaultContext default context}
     *
     * @return    the handle for the request
     *
     * @throws HttpException      if the request can not be dispatched
     * @throws IOException        in case of an IO problem
     */
    public HttpHandle sendRequest(HttpRequest req,
                                  HttpHost target,
                                  HttpContext ctxt)
        throws HttpException, IOException
        ;


    /**
     * Obtain the default context for {@link #sendRequest sending} requests.
     * The default context can be modified directly, or it can be cloned to
     * add request specific context information. Attributes that affect the
     * behaviour of the dispatcher should not be modified directly as long
     * as requests are being dispatched.
     *
     * @return    the default context
     */
    public HttpContext getDefaultContext()
        ;


    /**
     * Aborts all current requests.
     * This affects all requests for which the dispatcher still maintains
     * a handle. Any exceptions will be reported via the respective handle.
     */
    public void abortAll()
        ;


} // interface HttpDispatcher
