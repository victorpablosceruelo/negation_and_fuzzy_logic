/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/contrib/org/apache/http/async/contrib/routing/NotifiedServiceThread.java $
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

package org.apache.http.async.contrib.routing;


import java.util.LinkedList;

import org.apache.http.async.HttpHandle;



/**
 * Example thread for asynchronous notification.
 * This is an abstract base class used by
 * {@link RoutingAsyncGet RoutingAsyncGet}.
 * It is defined as a separate class to make re-use simpler for you.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public abstract class NotifiedServiceThread extends Thread {

    /** The shutdown indicator. */
    private boolean keep_running;

    /** The queue for passing handles. */
    private final LinkedList handle_queue;

    /** The monitor object for the {@link #handle_queue handle_queue}. */
    private final Object handle_queue_monitor =
        new String(super.toString()+".handle_queue_monitor");


    /**
     * Creates a new notified service thread.
     */
    protected NotifiedServiceThread() {

        handle_queue = new LinkedList();
        keep_running = true;
    }


    /**
     * Main loop of this service thread.
     * Calls {@link #processHandle processHandle} for each handle
     * that is queued, in order.
     */
    public final void run() {

        while (keep_running) {

            HttpHandle handle = null;
            synchronized (handle_queue_monitor) {

                while (keep_running && handle_queue.isEmpty()) {
                    try {
                        handle_queue_monitor.wait();
                    } catch (InterruptedException ix) {
                        // ignore, we'll keep waiting in the loop if required
                    }
                } // while queue empty

                if (!handle_queue.isEmpty())
                    handle = (HttpHandle) handle_queue.removeFirst();

            } // synchronized

            if (keep_running && (handle != null)) {

                try {
                    processHandle(handle);

                } catch (Exception x) {

                    try {
                        processException(handle, x);

                    } catch (Exception e) {
                        // This will only be called if the derived class
                        // screwed up exception handling. We can't let this
                        // fly through since the thread would die silently.
                        System.out.println(getClass() + ":" + getName() +
                                           ": exception handling failed");
                        e.printStackTrace(System.out);
                    }
                }
            } // if handle

        } // while running

    } // run


    /**
     * Shuts down this thread.
     * The thread will be terminated gracefully.
     */
    public final void shutdown() {

        keep_running = false;
        this.interrupt();

    } // shutdown


    /**
     * Queues a handle for processing by this thread.
     *
     * @param handle    the handle to process
     */
    public final void queueHandle(HttpHandle handle) {

        if (handle == null)
            throw new IllegalArgumentException("handle must not be null");

        synchronized (handle_queue_monitor) {
            handle_queue.add(handle);
            handle_queue_monitor.notifyAll();
        }

    } // queueHandle


    /**
     * Called once for each handle routed to this thread.
     *
     * @param handle    the handle
     *
     * @throws Exception        in case of a problem
     */
    protected abstract void processHandle(HttpHandle handle)
        throws Exception
        ;


    /**
     * Called for exceptions thrown by {@link #processHandle processHandle}.
     * This method can be overridden in derived classes for error handling.
     * The default implementation prints the stack trace to System.out.
     *
     * @param handle    the handle for which processing failed
     * @param dart      the exception raised while processing
     */
    protected void processException(HttpHandle handle, Exception dart) {

        System.out.println(getClass() + ":" + getName() +
                           ": exception while processing " + handle);
        if (dart != null)
            dart.printStackTrace(System.out);
        else
            System.out.println(getClass() + ":" + getName() +
                               ": <exception is null>");

    } // processException


} // class NotifiedServiceThread
