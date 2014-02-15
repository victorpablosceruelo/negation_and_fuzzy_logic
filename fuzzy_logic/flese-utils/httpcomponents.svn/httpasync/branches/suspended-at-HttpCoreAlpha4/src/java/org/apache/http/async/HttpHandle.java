/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/HttpHandle.java $
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

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpException;
import org.apache.http.protocol.HttpContext;



/**
 * Represents a {@link HttpDispatcher dispatched} HTTP request.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public interface HttpHandle {


    /**
     * Obtain the request.
     * The request MUST NOT be modified while it is under control
     * of the dispatcher.
     *
     * @return    the request to this handle
     */
    public HttpRequest getRequest()
        ;


    /**
     * Obtain the response for the request, if there is one.
     * This method does not block until the response is available.
     * Use {@link #awaitResponse awaitResponse} to wait for the response.
     * Use {@link #checkError checkError} to check for problems that
     * may prevent the response from ever becoming available.
     *
     * @return    the response to the request, or
     *            <code>null</code> if it is not (yet) available
     */
    public HttpResponse getResponse()
        ;


    /**
     * Obtain the context for the request.
     * The context returned here is usually not the same that was passed to
     * {@link HttpDispatcher#sendRequest HttpDispatcher.sendRequest},
     * but the attributes in that context are available in this one, too.
     *
     * @return    the context for executing the request,
     *            never <code>null</code>
     */
    public HttpContext getContext()
        ;


    /**
     * Check whether this handle is still linked to the dispatcher.
     * A handle is linked from the time it is created, until the
     * response has been received in it's entirety, or until
     * {@link #close close} is called.
     * The link can be broken prematurely by error conditions, or
     * by calling {@link #abort abort}.
     * <br/>
     * A handle that is not linked will no longer be recognized by
     * the dispatcher, but the getters will still work.
     *
     * @return  <code>true</code> if this handle is still linked,
     *          <code>false</code> otherwise
     */
    public boolean isLinked()
        ;


    /**
     * Obtain the response for the request.
     * This method blocks until the response is available, or until
     * there is an {@link #checkError error} condition.
     *
     * @return    the response to the request
     *
     * @throws HttpException
     *    if there will be no response, for example because there
     *    was a problem executing the request
     * @throws IOException
     *    in case of an IO problem
     * @throws InterruptedException
     *    if the thread was interrupted while awaiting the response
     */
    public HttpResponse awaitResponse()
        throws HttpException, IOException, InterruptedException
        ;


    /**
     * Close this handle.
     * This indicates that the application is done retrieving the response,
     * and the underlying connection can now be used by the dispatcher.
     * The application does not have to read the response entity completely
     * before closing the handle.
     * <br/>
     * After the call, this handle is no longer {@link #isLinked linked}.
     * Unlike {@link #abort abort}, this method does not cancel request
     * execution. Neither does it prevent connection keep-alive.
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    public void close()
        throws HttpException, IOException
        ;


    /**
     * Abort the request execution and {@link #close close} this handle.
     * If the request is not yet dispatched, it will be cancelled before
     * being sent. If it has been sent but the response is not yet received,
     * the connection over which it was sent will be closed before the
     * response is received. If the request has already been executed,
     * the response is discarded.
     * <br/>
     * Note that aborting a request can affect other requests to the
     * same host if pipelining is used. They may have to be repeated.
     */
    public void abort()
        ;


    /**
     * Check for problems during request execution.
     * If there is no problem, this method simply returns.
     *
     * @throws HttpException
     *    if there was a logical problem executing the request
     * @throws IOException
     *    if there was an IO problem executing the request
     */
    public void checkError()
        throws HttpException, IOException
        ;

} // interface HttpHandle
