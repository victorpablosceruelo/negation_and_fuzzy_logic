/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/examples/org/apache/http/examples/NotifiedAsyncGet.java $
 * $Revision: 505896 $
 * $Date: 2007-02-11 12:34:48 +0100 (Sun, 11 Feb 2007) $
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

package org.apache.http.examples;


import java.util.LinkedList;

import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpHost;
import org.apache.http.HttpResponse;
import org.apache.http.HttpVersion;
import org.apache.http.async.AsyncHttpProcessor;
import org.apache.http.async.HttpAsyncClientConnection;
import org.apache.http.async.HttpDispatcher;
import org.apache.http.async.HttpHandle;
import org.apache.http.async.HttpNotificationHandler;
import org.apache.http.async.impl.SimpleHttpAsyncClientConnection;
import org.apache.http.async.impl.SimpleHttpDispatcher;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.message.HttpGet;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.protocol.BasicHttpProcessor;
import org.apache.http.protocol.RequestConnControl;
import org.apache.http.protocol.RequestContent;
import org.apache.http.protocol.RequestTargetHost;
import org.apache.http.protocol.RequestUserAgent;
import org.apache.http.util.EntityUtils;



/**
 * Example for using asynchronous {@link HttpNotificationHandler notification}.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 505896 $ $Date: 2007-02-11 12:34:48 +0100 (Sun, 11 Feb 2007) $
 * 
 * @since 4.0
 */
public class NotifiedAsyncGet {

    /** The name of a context attribute for passing a throwable. */
    public final static String PROBLEM_ATTRIBUTE = "problem";


    /** The dispatcher. */
    private HttpDispatcher http_dispatcher;

    /** The notification handler. */
    private NotifHandlerImpl notification_handler;

    /** The counter for processed handles. */
    private int processed_counter;

    /** The queue for passing handles. */
    private final LinkedList handle_queue;

    /**
     * The internal monitor object.
     * Used for synchronizing access to the {@link #handle_queue handle_queue}.
     */
    private final Object handle_queue_monitor =
        new String(getClass()+".handle_queue_monitor");


    /**
     * Main entry point to this example.
     *
     * @param args        command line arguments
     *
     * @throws Exception        in case of a problem
     */
    public static void main(String[] args) throws Exception {

        String[] targets = args;
        if ((targets == null) || (targets.length < 1)) {
            targets = new String[] {
                "/",
                "/servlets-examples/servlet/RequestInfoExample", 
                "/somewhere%20in%20pampa"
            };
        }

        NotifiedAsyncGet example = new NotifiedAsyncGet();

        example.prepareExample();
        example.executeExample(targets);
        example.cleanupExample();

    } // main


    /**
     * Default constructor for this example.
     */
    private NotifiedAsyncGet() {

        handle_queue = new LinkedList();
        processed_counter = 0;

    } // constructor


    /**
     * Prepare this example.
     *
     * @throws Exception        in case of a problem
     */
    private void prepareExample()
        throws Exception {

        http_dispatcher = createDispatcher();
        System.out.println("dispatcher " + http_dispatcher + "\n");

        notification_handler = new NotifHandlerImpl();
        System.out.println("notification handler " + notification_handler);
        http_dispatcher.getDefaultContext().setAttribute
            (HttpNotificationHandler.CTXT_NOTIFICATION_HANDLER,
             notification_handler);

    } // prepareExample


    /**
     * Execute a series of requests.
     *
     * @param targets   the URIs to request
     *
     * @throws Exception        in case of a problem
     */
    private void executeExample(String[] targets)
        throws Exception {

        if ((targets == null) || (targets.length < 1))
            throw new IllegalArgumentException
                ("targets must not be null nor empty");

        HttpHost     host    = new HttpHost("localhost", 8080);
        HttpHandle[] handles = new HttpHandle[targets.length];

        for (int i = 0; i < targets.length; i++) {

            HttpGet request = new HttpGet(targets[i]);
            System.out.println(">> Request URI: " +
                               request.getRequestLine().getUri());
            handles[i] = http_dispatcher.sendRequest(request, host, null);
            System.out.println(">> Handle: " + handles[i]);
            System.out.println("==============");

        } // for targets

        // In this example, the array of handles is not needed at all since
        // we'll get notifications for each handle. In a real application,
        // you might want to track the handles. There probably is some
        // application context for each request which needs tracking anyway.

        // now pick up handles as the notifications come in

        while (processed_counter < targets.length) {

            HttpHandle handle = null;

            synchronized (handle_queue_monitor) {

                try {
                    handle_queue_monitor.wait();
                } catch (InterruptedException ix) {
                    // ignore, we'll just repeat the loop if necessary
                }

                if (!handle_queue.isEmpty())
                    handle = (HttpHandle) handle_queue.removeFirst();

            } // synchronized

            // If we got a handle, process it. Then check for more handles.
            // Note that processing is *not* part of the synchronized block!
            while (handle != null) {

                processHandle(handle);
                processed_counter++;
                handle = null;

                synchronized (handle_queue_monitor) {
                    if (!handle_queue.isEmpty())
                        handle = (HttpHandle) handle_queue.removeFirst();
                }
            } // while handle

            System.out.println("== main thread: counter is " +
                               processed_counter);

        } // while processed counter

        System.out.println("\ndispatcher " + http_dispatcher + "\n");

    } // executeExample


    /**
     * Clean up this example.
     *
     * @throws Exception        in case of a problem
     */
    private void cleanupExample()
        throws Exception {

        // currently no cleanup necessary

    } // cleanupExample


    /**
     * Instantiate a dispatcher.
     *
     * @return    the dispatcher
     */
    private final static HttpDispatcher createDispatcher() {

        HttpParams params = new BasicHttpParams(null);
        HttpProtocolParams.setVersion(params, HttpVersion.HTTP_1_1);
        HttpProtocolParams.setContentCharset(params, "UTF-8");
        HttpProtocolParams.setUserAgent(params, "Jakarta-HttpComponents/1.1");
        HttpProtocolParams.setUseExpectContinue(params, false);

        HttpAsyncClientConnection conn = new SimpleHttpAsyncClientConnection();

        BasicHttpProcessor dhp = new BasicHttpProcessor();
        // Required request interceptors
        dhp.addInterceptor(new RequestContent());
        dhp.addInterceptor(new RequestTargetHost());
        // Recommended request interceptors
        dhp.addInterceptor(new RequestConnControl());
        dhp.addInterceptor(new RequestUserAgent());
        // not supported: dhp.addInterceptor(new RequestExpectContinue());

        AsyncHttpProcessor proc = new AsyncHttpProcessor(dhp);
        proc.setParams(params);

        ConnectionReuseStrategy crs = new DefaultConnectionReuseStrategy();

        HttpDispatcher hdp = new SimpleHttpDispatcher(conn, proc, crs);

        return hdp;

    } // createDispatcher


    /**
     * Called by the main application thread, once for each notification.
     * The notification handler passes the handles to the main application
     * thread, which then calls this method.
     *
     * @param handle    the handle
     *
     * @throws Exception  in case of a problem
     */
    private void processHandle(HttpHandle handle)
        throws Exception {

        // check for an error first
        try {
            handle.checkError();
        } catch (Exception x) {
            System.out.print("!! Problem: ");
            x.printStackTrace(System.out);
            return; // no need to close the handle
        }

        // we know there is no error, so the response can not be null
        HttpResponse response = handle.getResponse();
        System.out.println("<< Response: " + response.getStatusLine());

        boolean readall = false; // change this to read only partially
        String  entity = null;
        if (readall) {
            entity = EntityUtils.toString(response.getEntity());
        } else {
            byte[] data = new byte[100];
            int count = response.getEntity().getContent().read(data);
            entity = new String(data, 0, count, "ISO-8859-1");
        }
        System.out.println(entity);

        // don't forget this, or the connection remains locked!
        handle.close();

    } // processHandle



    /**
     * The notification handler.
     * This class is public only to give you easier access to the JavaDoc.
     */
    public final class NotifHandlerImpl
        implements HttpNotificationHandler {

        // default constructor


        /**
         * Queues a handle for the main application thread.
         *
         * @param handle    the handle to process in the application thread
         *
         */
        private final void queueHandle(HttpHandle handle) {

            if (handle == null)
                throw new IllegalArgumentException("handle must not be null");

            synchronized (handle_queue_monitor) {
                handle_queue.add(handle);
                handle_queue_monitor.notifyAll();
            }

        } // queueHandle


        /**
         * Called when a response is available.
         * Accessing the response entity is a big no-no during notification.
         * We must not call:
         * <ul>
         * <li>{@link HttpHandle#getResponse handle.getResponse}
         *      </li>
         * <li>{@link HttpHandle#awaitResponse handle.awaitResponse}
         *      </li>
         * <li>{@link HttpResponse#getEntity response.getEntity}
         *      </li>
         * <li>{@link HttpResponse#setEntity response.setEntity}
         *      </li>
         * <li>{@link HttpHandle#checkError handle.checkError}
         *      (should not)</li>
         * </ul>
         * Of course we're not supposed to modify the <code>response</code>
         * at all, though that is not a strict requirement. But if we have
         * to pass additional information to the application thread, we can
         * simply put it in the {@link HttpHandle#getContext context}.
         *
         * @param handle        the handle for which the response is available.
         * @param nqrsp         (not quite) the available response
         */
        public void notifyResponse(HttpHandle handle, HttpResponse nqrsp) {

            System.out.println("== handler: notification for " + handle);

            // we could check for example the response status code to
            // select between multiple routing targets for responses

            queueHandle(handle);

        } // notifyResponse


        /**
         * Called when a problem is detected.
         * We must not call:
         * <ul>
         * <li>{@link HttpHandle#getResponse handle.getResponse}
         *      </li>
         * <li>{@link HttpHandle#awaitResponse handle.awaitResponse}
         *      </li>
         * </ul>
         * We should not call {@link HttpHandle#checkError handle.checkError}.
         * The exception we could get from there is already passed as argument.
         *
         * @param handle    the handle for which a problem has occurred
         * @param problem   the exception or throwable indicating the problem,
         *                  never <code>null</code>
         * @param nonfatal  <code>true</code> if the problem is non-fatal, or
         *                  <code>false</code> if request processing must be
         *                  aborted due to the problem
         *
         * @return Will be ignored for fatal problems.
         *      In case of a non-fatal problem,
         *      <code>true</code> if processing should resume or
         *      <code>false</code> if processing should be aborted.
         */
        public boolean notifyProblem(HttpHandle handle, Throwable problem,
                                     boolean nonfatal) {

            System.out.println("== handler: problem notification for " +
                               handle);

            queueHandle(handle);

            return false;

        } // notifyProblem

    } // class NotifHandlerImpl


} // class NotifiedAsyncGet
