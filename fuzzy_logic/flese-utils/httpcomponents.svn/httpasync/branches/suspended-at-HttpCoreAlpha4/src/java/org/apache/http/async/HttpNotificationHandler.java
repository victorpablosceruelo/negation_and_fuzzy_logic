/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/HttpNotificationHandler.java $
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


import org.apache.http.HttpResponse;
import org.apache.http.protocol.HttpContext;



/**
 * Callback interface for notifications about asynchronous requests.
 * Callback methods are typically called by background threads of a
 * {@link HttpDispatcher dispatcher}. To ensure correct operation of
 * those background threads, notification handler implementations
 * MUST conform to the following restrictions when processing
 * notification callbacks:
 * <ol>
 * <li>Notifications MUST be processed quickly.
 *     <br/>
 *     A handler method is not allow to perform expensive computations
 *     or long-lasting I/O operations. Most definitely, it is not allowed
 *     to perform user interaction, like popping up dialogs for accepting
 *     cookies or entering passwords.
 *     </li>
 * <li>A handler method MUST NOT access the response entity.
 *     <br/>
 *     This is to ensure that the notification is processed quickly.
 *     </li>
 * <li>A handler method MUST NOT close the handle.
 *     <br/>
 *     Closing a handle implies skipping (and therefore accessing)
 *     the response entity in order to re-use the connection.
 *     </li>
 * </ol>
 * Typically, a notification handler implementation will only pass the
 * handles to an application thread by some application specific means,
 * for example queues. The application thread then has full access to
 * the response and entity.
 * In order to simplify the application design, a notification handler
 * can perform simple routing operations. The routing decision can be
 * based on data in the {@link HttpHandle#getContext context}, on the
 * status code, and on the headers of the response.
 * For example, handles for which a {@link #notifyProblem problem} is
 * indicated can be routed to a different queue than handles for which a
 * {@link #notifyResponse response} with an error status code is received,
 * while handles for responses with non-error status codes are routed
 * to a third queue.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public interface HttpNotificationHandler {

    /**
     * The context attribute name for a notification handler.
     * Store your handler instance in the {@link HttpContext HttpContext}
     * for the {@link HttpHandle#getContext request} or the
     * {@link HttpDispatcher#getDefaultContext default} context
     * for the dispatcher.
     */
    public final static String CTXT_NOTIFICATION_HANDLER =
        HttpContext.RESERVED_PREFIX + "notification.handler";


    /**
     * Notification that a response is available.
     * <br/>
     * The response object passed as argument is not necessarily the
     * same object that will be returned by
     * {@link HttpHandle#getResponse handle.getResponse()}.
     * The notification handler is <i>not</i> allowed to call that
     * method, it must use the passed response object. The notification
     * handler is <i>not</i> allowed to access the response entity.
     * The passed response object is valid only during the notification,
     * it MUST NOT be passed to a different thread or stored for later
     * access.
     *
     * @param handle    the handle for which a response is now available
     * @param nqrsp     not quite the response that the application will get.
     *                  Application threads are allowed to call
     *                  {@link HttpHandle#getResponse handle.getResponse()},
     *                  which returns a postprocessed response.
     *                  A notification handler is not allowed to call that
     *                  method and has to use this argument instead. It may
     *                  or may not be postprocessed. It does not give access
     *                  to the response entity, if there is one. It does give
     *                  access to the status line and headers.
     */
    public void notifyResponse(HttpHandle handle, HttpResponse nqrsp)
        // no exceptions allowed
        ;


    /**                               
     * Notification that a problem has occurred.
     * Some problems are non-fatal, for example because the request can
     * be retried. The <code>nonfatal</code> argument indicates this case
     * to the handler method, and the return value instructs the dispatcher
     * whether to continue request processing or whether to abort. If the
     * problem is fatal, request processing is always aborted.
     *
     * @param handle    the handle for which a problem has occurred
     * @param problem   the exception or throwable indicating the problem,
     *                  never <code>null</code>
     * @param nonfatal  <code>true</code> to indicate that request processing
     *                  <i>can</i> proceed if requested, or
     *                  <code>false</code> if request processing needs to be
     *                  aborted due to the problem
     *
     * @return  <code>true</code> to indicate that request processing should
     *          proceed in spite of the problem, or <code>false</code> if
     *          request processing should be aborted.
     *          The returned value will be ignored if the <code>nonfatal</code>
     *          argument is false.
     *          If the request processing proceeds, there will be another
     *          notification for the same handle, either when a
     *          {@link #notifyResponse response} is available or
     *          when another problem is encountered. Otherwise, the handle
     *          is closed and there will be no further notification.
     */
    public boolean notifyProblem(HttpHandle handle, Throwable problem,
                                 boolean nonfatal)
        // no exceptions allowed
        ;


} // interface HttpNotificationHandler
