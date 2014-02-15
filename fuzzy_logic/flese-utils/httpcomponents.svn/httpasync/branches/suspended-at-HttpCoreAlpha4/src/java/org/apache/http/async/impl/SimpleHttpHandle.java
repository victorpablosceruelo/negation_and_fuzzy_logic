/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/impl/SimpleHttpHandle.java $
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

package org.apache.http.async.impl;

import java.io.IOException;

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpException;
import org.apache.http.protocol.HttpContext;
import org.apache.http.async.AbstractHttpHandle;
import org.apache.http.async.HttpNotificationHandler;



/**
 * Handles for the {@link SimpleHttpDispatcher SimpleHttpDispatcher}.
 *
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public class SimpleHttpHandle extends AbstractHttpHandle {
    // Public and non-final because SimpleHttpDispatcher is not final.
    // Derived dispatchers may have to instantiate or derive the handle class.


    // a selection of possible response states
    protected final static int RESPONSE_STATE_MISSING  = 0; // not received yet
    protected final static int RESPONSE_STATE_PRESENT  = 1; // just received
    protected final static int RESPONSE_STATE_FINISHED = 2; // post-processed


    /** The response. It may or may not be postprocessed. */
    HttpResponse response_object;

    /** The state of the response. */
    private int response_state;

    /** The monitor for synchronizing access to the response. */
    private final Object response_monitor =
        new String("SimpleHttpHandle.response_monitor");

    /**
     * The thread used for a notification callback.
     * <code>null</code> if no notification is in progress.
     * This attribute should be read only while a lock on the
     * {@link #response_monitor response_monitor} is held.
     */
    private Thread notification_thread;



    /**
     * Create a new handle.
     *
     * @param dispatcher  the dispatcher creating this handle
     * @param request     the request to be handled
     * @param context     the context for executing the request
     */
    /*default*/ SimpleHttpHandle(SimpleHttpDispatcher dispatcher,
                                 HttpRequest request,
                                 HttpContext context) {

        super(dispatcher, request, context);

        if (dispatcher == null)
            throw new IllegalArgumentException("dispatcher must be specified");

        // no need to synchronize in constructor
        response_state = RESPONSE_STATE_MISSING;

    } // constructor



    // non-javadoc, see interface HttpHandle
    public final HttpResponse getResponse() {

        checkThreadAbuse();

        // if the response is pending post-processing,
        // use the calling thread to do it now
        synchronized (response_monitor) {
            try {
                if (response_state == RESPONSE_STATE_PRESENT)
                    postprocessResponse();

            } catch (Throwable t) {
                response_object = null;
                internalProvideProblem(t, false, false);
            }
        } // synchronized response_monitor

        return response_object;

    } // getResponse


    // non-javadoc, see interface HttpHandle
    public final HttpResponse awaitResponse()
        throws HttpException, IOException, InterruptedException {

        checkThreadAbuse();

        synchronized (response_monitor) {
            while (response_state == RESPONSE_STATE_MISSING) {
                response_monitor.wait();
            }
            // use calling thread to perform post-processing
            getResponse();
        }

        return response_object;

    } // awaitResponse


    /**
     * Obtain the response object, if available.
     * For use by the {@link SimpleHttpDispatcher dispatcher} only.
     * This method does not synchronize the access, nor check a state,
     * nor perform postprocessing.
     *
     * @return  the response object, or <code>null</code> if not available
     */
    protected final HttpResponse internalGetResponse() {

        return response_object;
    }


    // non-javadoc, see interface HttpHandle
    public synchronized void close()
        throws HttpException, IOException {

        checkThreadAbuse();
        if (!isLinked())
            return;

        setLinked(false);
        dispatcherCloseHandle(false);

        synchronized (response_monitor) {
            // handle remains useable if there is a response
            if (response_state == RESPONSE_STATE_MISSING) {
                internalProvideProblem(new IllegalStateException("closed"),
                                       false, false);
            }
        }

    } // close


    // non-javadoc, see interface HttpHandle
    public void abort() {

        if (!isLinked())
            return;

        setLinked(false);
        try {
            dispatcherCloseHandle(true);
        } catch (Exception x) {
            // ignore: sanity checks will be passed, nothing else is expected
            // to fail, caller is not interested in handling problems anyway
            //@@@ TODO: log problem - just in case there is an unexpected one
        }

        synchronized (response_monitor) {
            // handle no longer useable
            response_object = null;
            internalProvideProblem(new IllegalStateException("aborted"),
                                   false, false);
        }

    } // abort


    /**
     * Provide the response for this handle.
     * This method is called by the background thread of the
     * {@link SimpleHttpDispatcher SimpleHttpDispatcher}.
     *
     * @param response    the response, received but not post-processed
     */
    public final void provideResponse(HttpResponse response) {

        synchronized (response_monitor) {
            response_object = response;
            response_state = RESPONSE_STATE_PRESENT;
            response_monitor.notifyAll();
        }

        notifyResponse();

    } // provideResponse


    /**
     * Notify the callback handler about the response.
     * Called from {@link #provideResponse provideResponse}.
     */
    private final void notifyResponse() {

        if (notification_handler == null)
            return;

        HttpResponse notifyrsp =
            new NotificationResponseWrapper(response_object);

        try {
            synchronized (response_monitor) {
                // enable protection against thread abuse
                notification_thread = Thread.currentThread();
                notification_handler.notifyResponse(this, notifyrsp);
            }

        } catch (Throwable t) {
            //@@@ TODO: log the problem
//System.out.println("@@@ notification handler triggered exception");
//t.printStackTrace(System.out); //@@@
        } finally {
            notification_thread = null;
        }

    } // notifyResponse


    /**
     * Provide the problem for this handle.
     * A problem means there will be no {@link #provideResponse response}.
     * This method is called by the background thread of the
     * {@link SimpleHttpDispatcher SimpleHttpDispatcher}.
     * The handle becomes {@link AbstractHttpHandle#isLinked unlinked}.
     *
     * @param dart      the exception indicating the problem,
     *                  or <code>null</code> to indicate an unknown problem
     * @param notify    <code>true</code> to trigger a callback to the
     *                  notification handler, or
     *                  <code>false</code> if there should be no callback.
     *                  If <code>true</code>, the handle will also be unlinked.
     */
    public final void provideProblem(Throwable dart,
                                     boolean notify) {

        internalProvideProblem(dart, true, notify);
    }


    /**
     * Internally provide the problem for this handle.
     * A problem means there will be no {@link #provideResponse response}.
     *
     * @param dart      the exception indicating the problem,
     *                  or <code>null</code> to indicate an unknown problem
     * @param unlink    <code>true</code> to unlink this handle, or
     *                  <code>false</code> to keep linkage unchanged
     * @param notify    <code>true</code> to trigger a callback to the
     *                  notification handler, or
     *                  <code>false</code> if there should be no callback.
     *                  If <code>true</code>, the handle will also be unlinked.
     */
    private final void internalProvideProblem(Throwable dart,
                                              boolean unlink,
                                              boolean notify) {

        // don't throw an exception during exception handling!
        if (dart == null)
            dart = new HttpException("problem without exception");

        setError(dart);
        synchronized (response_monitor) {
            response_state = RESPONSE_STATE_FINISHED;
            response_monitor.notifyAll();
            if (unlink)
                setLinked(false);
        }

        if (notify)
            notifyProblem();

    } // internalProvideProblem


    /**
     * Notify the callback handler about a problem.
     * Called from {@link #internalProvideProblem internalProvideProblem}.
     * Unlike {@link HttpNotificationHandler#notifyProblem
     *               HttpNotificationHandler.notifyProblem},
     * this method does not have a return value nor boolean flag,
     * since it is called only for fatal problems.
     */
    private final void notifyProblem() {

        if (notification_handler == null)
            return;

        try {
            synchronized (response_monitor) {
                // enable protection against thread abuse
                notification_thread = Thread.currentThread();
                // ignore return value:
                notification_handler.notifyProblem(this, getError(), false);
            }

        } catch (Throwable t) {
            //@@@ TODO: log the problem
//System.out.println("@@@ notification handler triggered exception");
//t.printStackTrace(System.out); //@@@
        } finally {
            notification_thread = null;
        }

    } // notifyProblem


    /**
     * Post-process the response.
     * This will update the response object and state.
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    private final void postprocessResponse()
        throws HttpException, IOException {

        synchronized (response_monitor) {

            if (response_state != RESPONSE_STATE_PRESENT) {
                return; // Ha ha, very funny. Duh!
            }

            if (response_object != null)
                dispatcherPostprocess(response_object);

            response_state = RESPONSE_STATE_FINISHED;

        } // synchronized response_monitor

    } // postprocessResponse


    /**
     * Check for abuse of a notification thread.
     * Notification handlers are not allowed to call some methods of
     * this handle, to prevent them from blocking background threads
     * or triggering nested notifications.
     * This method throws an exception if thread abuse is detected,
     * and simply returns otherwise.
     *
     * @throws IllegalThreadStateException
     *          if the calling thread is a background thread currently
     *          executing a notification for this handle
     */
    private final void checkThreadAbuse()
        throws IllegalThreadStateException {

        // No need to synchronize access to the notification_thread here.
        // If it is set to the current thread, then it has been set by the
        // current thread, and operation within a thread is sequential.

        if (notification_thread == null)
            return;

        if (notification_thread == Thread.currentThread()) {
            final IllegalThreadStateException itsx =
                new IllegalThreadStateException("notification thread abuse");
            //@@@ TODO: log now, in case the caller catches the exception
            throw itsx;
        }

    } // checkThreadAbuse


} // class SimpleHttpHandle
