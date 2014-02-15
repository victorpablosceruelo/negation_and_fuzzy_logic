/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/AbstractHttpHandle.java $
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
import org.apache.http.HttpException;
import org.apache.http.HttpResponse;
import org.apache.http.protocol.HttpContext;

/**
 * Base class for implementations of {@link HttpHandle HttpHandle}.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public abstract class AbstractHttpHandle implements HttpHandle {

    /** The dispatcher which created this handle. */
    protected final AbstractHttpDispatcher http_dispatcher;

    /** The request being dispatched. */
    protected final HttpRequest http_request;

    /** The context for request execution. */
    protected final HttpContext http_context;

    /** The link status for this handle. */
    private boolean is_linked;

    /** The exception indicating an execution problem, if there was one. */
    private Throwable execution_problem;

    /** The notification handler, or <code>null</code>. */
    protected final HttpNotificationHandler notification_handler;


    /**
     * Create a new handle.
     * The new handle is {@link #isLinked linked}.
     *
     * @param dispatcher  the dispatcher creating this handle,
     *                    or <code>null</code>
     * @param request     the request to be handled
     * @param context     the context for executing the request
     */
    protected AbstractHttpHandle(AbstractHttpDispatcher dispatcher,
                                 HttpRequest request,
                                 HttpContext context) {
        if (request == null)
            throw new IllegalArgumentException("request must not be null");
        if (context == null)
            throw new IllegalArgumentException("context must not be null");

        http_dispatcher = dispatcher;
        http_request    = request;
        http_context    = context;
        is_linked       = true;

        // Look up the notification handler now.
        // Later changes to the context will be ignored.
        notification_handler = (HttpNotificationHandler)
            context.getAttribute(HttpNotificationHandler.
                                 CTXT_NOTIFICATION_HANDLER);

    } // constructor


    // non-javadoc, see interface HttpHandle
    public final HttpRequest getRequest() {

        return http_request;
    }


    // non-javadoc, see interface HttpHandle
    public final HttpContext getContext() {

        return http_context;
    }


    /**
     * Check whether this handle is still linked to the dispatcher.
     * This method needs to be synchronized since it is called by
     * background threads to detect {@link HttpHandle#close closed}
     * or {@link HttpHandle#abort aborted} requests.
     */
    public final synchronized boolean isLinked() {

        return is_linked;
    }


    /**
     * Set the linking status of this handle.
     * This method is not synchronized, but should only be called by
     * synchronized methods in derived classes. It is used to specify
     * the value returned by {@link #isLinked isLinked}.
     *
     * @param linked    <code>false</code> if this handle is no longer linked,
     *                  <code>true</code> otherwise
     */
    protected final void setLinked(boolean linked) {

        is_linked = linked;
    }



    // non-javadoc, see interface HttpHandle
    public final void checkError()
        throws HttpException, IOException {

        if (execution_problem == null)
            return;

        if (execution_problem instanceof HttpException) {
            throw (HttpException) execution_problem;

        } else if (execution_problem instanceof IOException) {
            throw (IOException) execution_problem;

        } else {
            HttpException hx = new HttpException
                ("problem while executing request");
            hx.initCause(execution_problem);
            throw hx;
        }
    } // checkError


    /**
     * Remember an execution problem.
     * This sets the error for which {@link #checkError checkError} is looking.
     *
     * @param dart        the exception indicating the problem,
     *                    or <code>null</code> to unset the problem
     */
    protected final void setError(Throwable dart) {

        execution_problem = dart;

    } // setError


    /**
     * Obtain the execution problem.
     *
     * @return    the last exception passed to {@link #setError setError},
     *            or <code>null</code>
     */
    public final Throwable getError() {

        return execution_problem;
    }


    /**
     * Obtain the notification handler.
     *
     * @return  the notification handler for the request, or
     *          <code>null</code> if there is none
     */
    public final HttpNotificationHandler getNotificationHandler() {

        return notification_handler;
    }


    /**
     * Check the dispatcher.
     * Simply returns if the argument is the same as the one
     * given to the constructor.
     * This does not check whether the handle is {@link #isLinked linked}.
     *
     * @param dispatcher  the dispatcher performing the check,
     *                    or <code>null</code>
     *
     * @throws IllegalArgumentException
     *    if this handle was not created for the argument dispatcher
     */
    public final void checkDispatcher(HttpDispatcher dispatcher) {

        if (http_dispatcher != dispatcher)
            throw new IllegalArgumentException
                ("handle used with wrong dispatcher");
    }


    /**
     * Callback to the dispatcher to close this handle.
     * Maps to
     * {@link AbstractHttpDispatcher#closeHandle http_dispatcher.closeHandle}.
     * Does nothing if the dispatcher has not been passed to the constructor.
     *
     * @param abort     <code>true</code> if the handle is closed because of
     *                  a call to {@link HttpHandle#abort abort}, or
     *                  <code>false</code> if it is closed because of
     *                  a call to {@link HttpHandle#close close}
     *                  
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected final void dispatcherCloseHandle(boolean abort)
        throws HttpException, IOException
        {
            if (http_dispatcher != null)
                http_dispatcher.closeHandle(this, abort);
        }


    /**
     * Callback to the dispatcher to postprocess the response.
     * Maps to {@link AbstractHttpDispatcher#postprocessResponse
     *                http_dispatcher.postprocessResponse}.
     * Does nothing if the dispatcher has not been passed to the constructor.
     *
     * @param response          the response to postprocess
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected final void dispatcherPostprocess(HttpResponse response)
        throws HttpException, IOException
        {
            if (http_dispatcher != null)
                http_dispatcher.postprocessResponse(this, response);
        }

} // class AbstractHttpHandle
