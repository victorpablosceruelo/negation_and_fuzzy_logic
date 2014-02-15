/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/impl/SimpleHttpDispatcher.java $
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
import java.util.List;
import java.util.LinkedList;
import java.util.Collection;

import org.apache.http.HttpRequest;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHost;
import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpException;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;
import org.apache.http.async.HttpAsyncClientConnection;
import org.apache.http.async.HttpHandle;
import org.apache.http.async.HttpDispatcher;
import org.apache.http.async.HttpDispatcherParams;
import org.apache.http.async.AsyncHttpProcessor;
import org.apache.http.async.AbstractHttpHandle;
import org.apache.http.async.AbstractHttpDispatcher;

/**
 * Minimal implementation of an {@link HttpDispatcher HttpDispatcher}.
 * This dispatcher uses a single connection and background thread.
 * Requests are dispatched in order. The background thread switches
 * from sending a request to receiving the response and notifying.
 * Request execution is therefore serialized.
 * <br/>
 * If synchronization has to be nested, the required locks MUST be
 * obtained in the following order to prevent deadlocks:
 * <ol>
 * <li><code>this</code></li>
 * <li>{@link AbstractHttpDispatcher#linked_handle_monitor
 *            linked_handle_monitor}</li>
 * <li>{@link #request_handle_monitor request_handle_monitor}</li>
 * <li>{@link #response_handle_monitor response_handle_monitor}</li>
 * </ol>
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 1.0
 */
public class SimpleHttpDispatcher extends AbstractHttpDispatcher
    implements HttpDispatcher {
    //@@@ move some of this stuff to an abstract base class?
    //@@@ Not the AsyncHttpProcessor and getDefaultContext method, that
    //@@@ would not allow for dispatching with different processors.

    private final HttpContext context;
    
    /** The one and only connection. */
    private final HttpAsyncClientConnection client_connection;

    /**
     * The HTTP processor.
     * For pre- and postprocessing of requests and responses, respectively.
     */
    protected final AsyncHttpProcessor async_processor;

    /** The connection re-use strategy. */
    private ConnectionReuseStrategy reuse_strategy;

    /** The background thread. */
    private SimpleHttpDispatcherThread background_thread;


    /** The monitor object for accessing the queue of request handles. */
    protected final Object request_handle_monitor =
        new String("SimpleHttpDispatcher.request_handle_monitor");

    /**
     * The queue of request handles to process.
     * Request handles are handles for requests that have not been sent yet.
     */
    private final List request_handle_queue;

    /** The monitor object for accessing the queue of response handles. */
    protected final Object response_handle_monitor =
        new String("SimpleHttpDispatcher.response_handle_monitor");

    /**
     * The collection of response handles.
     * Response handles are handles for requests that have been sent
     * or are being sent right now. Response handles are thrown away
     * only after the connection over which the response is received
     * is available again.
     * For the simple dispatcher, there is at most one response handle.
     */
    private final Collection response_handles;



    /**
     * Create a new simple HTTP dispatcher.
     *
     * @param conn      the connection to use
     * @param proc      the HTTP processor to use,
     *                  including the default context
     * @param reuse     the strategy for re-using connections, or
     *                  <code>null</code> to disable connection re-use
     */
    public SimpleHttpDispatcher(HttpAsyncClientConnection    conn,
                                AsyncHttpProcessor           proc,
                                ConnectionReuseStrategy      reuse) {
        super(null);

        if (conn == null)
            throw new IllegalArgumentException("connection must not be null");
        if (proc == null)
            throw new IllegalArgumentException("processor must not be null");

        client_connection = conn;
        async_processor   = proc;
        reuse_strategy    = reuse;
        context = new HttpExecutionContext(null);

        request_handle_queue = new LinkedList();
        response_handles     = new LinkedList();

        background_thread = new SimpleHttpDispatcherThread();
        background_thread.setDaemon(true);
        background_thread.start();

    } // constructor



    // non-javadoc, see interface HttpDispatcher
    public HttpHandle sendRequest(HttpRequest request,
                                  HttpHost target,
                                  HttpContext context)
        throws HttpException, IOException {

        if (request == null)
            throw new IllegalArgumentException("request must not be null");
        if (target == null)
            throw new IllegalArgumentException("target host must not be null");

        if ((request instanceof HttpEntityEnclosingRequest) &&
            ((HttpEntityEnclosingRequest)request).expectContinue()) {

            throw new IllegalArgumentException
                ("expect-continue not supported asynchronously");
        }

        //@@@ check whether target is suitable:
        //@@@ - for now: no proxy (how to indicate proxy in the first place?)

        //@@@ setting default parameters is also part of "prepareRequest",
        //@@@ but we need the params below to decide when to preprocess
        HttpParams params = request.getParams();
        params.setDefaults(async_processor.getParams());

        HttpContext ctxt = prepareContext(async_processor, target, context);
        SimpleHttpHandle hdl = new SimpleHttpHandle(this, request, ctxt);

        if (!HttpDispatcherParams.getPreprocessBackground(params))
            prepareRequest(async_processor, request, ctxt);

        // linked handles need to be tracked for abortAll()
        synchronized (linked_handle_monitor) {
            linked_handles.add(hdl);

            // Queue the handle, it will be picked up by the background thread.
            // Double-synchronized so abortAll() can't drop the handle from the
            // linked_handles before we insert it into the the request queue.
            synchronized (request_handle_monitor) {
                request_handle_queue.add(hdl);
                // tell the background thread
                request_handle_monitor.notifyAll();
            }
        }


        return hdl;

    } // sendRequest


    // non-javadoc, see base class AbstractHttpDispatcher
    protected void postprocessResponse(AbstractHttpHandle handle,
                                       HttpResponse response)
        throws HttpException, IOException {

        if (handle == null)
            throw new IllegalArgumentException("handle must not be null");
        if (response == null)
            throw new IllegalArgumentException("response must not be null");
        handle.checkDispatcher(this);

        finishResponse(async_processor, response, handle.getContext());

    } // postprocessResponse



    // non-javadoc, see interface HttpDispatcher
    public final HttpContext getDefaultContext() {

        //@@@ consider removing the default context from the HttpDispatcher 
        //@@@ interface altogether
        return context;
    }


    // non-javadoc, see base class AbstractHttpHandle
    protected void closeHandle(AbstractHttpHandle handle, boolean abort)
        throws HttpException, IOException {

        if (handle == null)
            throw new IllegalArgumentException("handle must not be null");
        handle.checkDispatcher(this);

        boolean found = false;
        synchronized (linked_handle_monitor) {
            found = linked_handles.remove(handle);
        }
        if (!found) {
            // unlinked handles are no concern anymore
            return;
        }

        // The handle might be in the request queue, the response collection,
        // or neither. There is also a small window in which the handle is
        // removed from the request queue by the background thread, but not
        // yet put into the response collection. Let the background thread
        // deal with that.

        synchronized (request_handle_monitor) {
            found = request_handle_queue.remove(handle);
        }

        if (!found) {
            synchronized (response_handle_monitor) {
                found = response_handles.remove(handle);
                try {
                    if (found) {
                        try {
                            if (abort)
                                client_connection.close();
                        } finally {
                            // Background thread can be waiting for the rsp,
                            // or for the completion of rsp processing.
                            // Either way, it needs to continue.
                            background_thread.interrupt();
                        }
                    }
                } finally {
                    response_handle_monitor.notifyAll();
                }
            } // synchronized response_handle_monitor
        }

    } // closeHandle


    /**
     * Gracefully closes a response.
     * This includes deciding on connection re-use and whether to
     * close the connection or read to the end of the response entity.
     * This method is called by the background thread.
     *
     * @param handle    the handle for which to close the response
     *
     * @throws IOException      in case of a problem closing the response
     */
    private final void closeResponse(SimpleHttpHandle handle)
        throws IOException {

        HttpResponse response = handle.internalGetResponse();
        boolean      reuse    = false;
        try {
            if ((response != null) && (reuse_strategy != null) &&
                reuse_strategy.keepAlive(response, handle.getContext())) {

                // consume rest of the entity, if there is one
                HttpEntity entity = response.getEntity();
                if (entity != null) {
                    entity.consumeContent();
                }
                reuse = true;
            }
        } finally {
            // This could trigger an exception during exception handling,
            // but we need to close the connection if it can't be re-used.
            if (!reuse)
                client_connection.close();
        }

    } // closeResponse


    // non-javadoc, see interface HttpDispatcher
    public void abortAll() {

        // this will prevent interference from newly dispatched requests
        synchronized (linked_handle_monitor) {

            // first, clear the request queue so handles can't be picked up
            synchronized (request_handle_monitor) {
                request_handle_queue.clear();
            }

            // Now abort each handle individually. This cleans up handles in
            // the response collection and ensures application notification.
            // Keep this in the synchronized block to make sure no new handles
            // are created and possibly picked up until all current ones have
            // been aborted. The collection is copied for stable iteration.

            HttpHandle[] handles = (HttpHandle[])
                linked_handles.toArray(new HttpHandle[linked_handles.size()]);

            for (int i=0; i<handles.length; i++) {
                if (handles[i] != null) // you never know
                    handles[i].abort();
            }
        } // synchronized linked_handle_monitor

    } // abortAll





    /**
     * Background thread for the simple dispatcher.
     * This needs to be an inner class, since the monitors
     * and handle queues should be private.
     *
     *
     * <!-- empty lines above to avoid 'svn diff' context problems -->
     * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
     * 
     * @since 1.0
     */
    private class SimpleHttpDispatcherThread extends Thread {

        /**
         * Main loop of this background dispatch thread.
         * Waits for a request to dispatch, sends the request,
         * waits and sets up the response, waits for completion of
         * response processing, and starts again.
         */
        public void run() {

            while (background_thread == this) {

                SimpleHttpHandle handle = null;
                boolean          notify = true;
                try {
                    handle = awaitRequestHandle();
                    if (handle != null) {

                        synchronized (response_handle_monitor) {
                            // the check for stale handles must be performed
                            // within this synchronized block, for proper
                            // synchronization with closeHandle()
                            if (!handle.isLinked())
                                continue; // while (background_thread)

                            response_handles.add(handle);
                        }

                        // check whether preprocessing is required
                        if (!Boolean.TRUE.equals
                            (handle.getContext().getAttribute
                             (CTXT_REQUEST_PREPROCESSED))) {

                            prepareRequest(async_processor,
                                           handle.getRequest(),
                                           handle.getContext());
                        }

                        //@@@ TODO: retry handling!
                        transmitRequestTo(async_processor,
                                          handle.getRequest(),
                                          handle.getContext(),
                                          client_connection);

                        //@@@ TODO: timeout handling!
                        HttpResponse response = 
                            obtainResponse(async_processor,
                                           handle.getRequest(),
                                           handle.getContext(),
                                           client_connection);
/*@@@ hack to test problem notification
if (response.getStatusLine().getStatusCode() != 200)
    throw new IOException("fake IO problem");
@@@ end of hack to test problem notification */

                        // providing the response to the handle triggers the
                        // response notification: disable problem notification
                        handle.provideResponse(response);
                        notify = false;

                        if (response.getEntity() != null) {
                            awaitCompletion(handle);
                            closeResponse(handle);
                        } else {
                            //@@@ close/unlink the handle?
//@@@ calling close() on the handle would interrupt this thread - not good
                        }

                    } // if request handle

                } catch (Throwable t) {
                    cleanupOnProblem(handle, t, notify);
                }
            } // while thread not cancelled

        } // run


        /**
         * Clean up after a problem was encountered.
         * The problem is indicated via the handle, if there is one.
         * The connection is shut down to establish a defined state.
         * The handle is unlinked.
         *
         * @param handle    the handle for which there was a problem,
         *                  or <code>null</code> if there is none
         * @param dart      the exception indicating the problem
         * @param notify    <code>true</code> if a problem notification should
         *                  be triggered, or <code>false</code> if the response
         *                  notification has already been triggered
         */
        private void cleanupOnProblem(SimpleHttpHandle handle,
                                      Throwable dart,
                                      boolean notify) {

            //@@@ TODO: log the problem, in case it isn't picked up by the app

            if (handle == null) {
                // No handle means no request, and nothing can have been sent
                // via the connection. No need to shut it down in this case.
                return;
            }

            synchronized (response_handle_monitor) {
                response_handles.remove(handle);
            }
            synchronized (linked_handle_monitor) {
                linked_handles.remove(handle);
            }
            handle.provideProblem(dart, notify);

            try {
                client_connection.shutdown();
            } catch (IOException iox) {
                //@@@ TODO: log the problem
//System.out.println("@@@ exception shutting down connection");
//iox.printStackTrace(System.out); //@@@
            }

        } // cleanupOnProblem


        /**
         * Wait for a request handle.
         * That's a handle from the queue of requests that need to be sent.
         *
         * @return  the handle of the request to send, or
         *          <code>null</code> if the thread has been cancelled
         */
        private SimpleHttpHandle awaitRequestHandle() {

            SimpleHttpHandle handle = null;
            synchronized (request_handle_monitor) {

                while (request_handle_queue.isEmpty() &&
                       (background_thread == this)) {

                    try {
                        request_handle_monitor.wait();

                    } catch (InterruptedException ix) {
                        // ignore and re-evaluate the while(...) condition
                    }
                } // while

                if (!request_handle_queue.isEmpty())
                    handle = (SimpleHttpHandle) request_handle_queue.remove(0);

            } // synchronized

            return handle;

        } // awaitRequestHandle



        /**
         * Wait for a response handle to be closed.
         * This indicates that the connection can be used for the next request.
         *
         * @return  the handle of the request to send, or
         *          <code>null</code> if the thread has been cancelled
         */
        private void awaitCompletion(SimpleHttpHandle handle) {

            synchronized (response_handle_monitor) {

                while (response_handles.contains(handle) &&
                       (background_thread == this)) {

                    try {
                        response_handle_monitor.wait();

                    } catch (InterruptedException ix) {
                        // ignore and re-evaluate the while(...) condition
                    }
                } // while
            } // synchronized response_handle_monitor

        } // awaitCompletion

    } // class SimpleHttpDispatcherThread


    /**
     * Create a human-readable description of this object.
     * Used for debug output.
     *
     * @return  string description of this object
     */
    public String toString() {

        StringBuffer sb = new StringBuffer(super.toString());

        synchronized (linked_handle_monitor) {
            synchronized (request_handle_monitor) {
                synchronized (response_handle_monitor) {
                    sb.append("\nlinked handles: ")
                        .append(linked_handles)
                        .append("\nrequest handles: ")
                        .append(request_handle_queue)
                        .append("\nresponse handles: ")
                        .append(response_handles);
                }
            }
        }

        return sb.toString();

    } // toString


} // class SimpleHttpDispatcher
