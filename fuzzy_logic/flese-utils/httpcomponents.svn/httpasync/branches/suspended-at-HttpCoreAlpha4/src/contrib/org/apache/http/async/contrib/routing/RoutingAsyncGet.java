/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/contrib/org/apache/http/async/contrib/routing/RoutingAsyncGet.java $
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

package org.apache.http.async.contrib.routing;


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
 * Example for routing handles on asynchronous notification.
 * Unlike the <code>NotifiedAsyncGet</code> example, this
 * one uses multiple service threads. The notification
 * {@link HttpNotificationHandler handler} decides which
 * of these service threads is responsible to process a
 * response or problem.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 505896 $ $Date: 2007-02-11 12:34:48 +0100 (Sun, 11 Feb 2007) $
 * 
 * @since 4.0
 */
public class RoutingAsyncGet {

    /** The name of a context attribute for passing a throwable. */
    public final static String PROBLEM_ATTRIBUTE = "problem";


    /** The dispatcher. */
    private HttpDispatcher http_dispatcher;

    /** The notification handler. */
    private NotifHandlerImpl notification_handler;

    /** The notification counter. */
    private int notification_counter;

    /** The monitor object for the notification counter. */
    private final Object notification_counter_monitor =
        new String(getClass() + ".notification_counter_monitor");


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

        RoutingAsyncGet example = new RoutingAsyncGet();

        example.prepareExample();
        example.executeExample(targets);
        example.cleanupExample();

    } // main


    /**
     * Default constructor for this example.
     */
    private RoutingAsyncGet() {

        notification_counter = 0;

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

        notification_handler = createNotificationHandler();
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

        // now wait until all handles have been processed by the threads...
        synchronized (notification_counter_monitor) {

            while (notification_counter < targets.length) {
                try {
                    notification_counter_monitor.wait();
                } catch (InterruptedException ix) {
                    // ignore, we'll just keep waiting in the loop
                }
                System.out.println("== main thread: notification counter is " +
                                   notification_counter);
            }

        } // synchronized

        System.out.println("\ndispatcher " + http_dispatcher + "\n");

    } // executeExample


    /**
     * Clean up this example.
     *
     * @throws Exception        in case of a problem
     */
    private void cleanupExample()
        throws Exception {

        shutdownThreads(notification_handler);

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
     * Start application threads and instantiates a notification handler.
     *
     * @return  the notification handler
     */
    private final NotifHandlerImpl createNotificationHandler() {

        ResponseThread rspthread = new ResponseThread();
        ProblemThread  plmthread = new ProblemThread();

        rspthread.setDaemon(true);
        rspthread.start();
        plmthread.setDaemon(true);
        plmthread.start();

        return new NotifHandlerImpl(rspthread, plmthread);

    } // createNotificationHandler


    /**
     * Stop the application threads.
     *
     * @param nhi       the notification handler implementation
     */
    private final static void shutdownThreads(NotifHandlerImpl nhi) {

        nhi.response_target.shutdown();
        nhi.problem_target.shutdown();

    } // shutdownThreads


    /**
     * The application thread for responses.
     */
    public final class ResponseThread extends NotifiedServiceThread {

        // default constructor

        /**
         * Called once for each handle routed to this thread.
         *
         * @param handle    the handle
         *
         * @throws Exception  in case of a problem
         */
        protected void processHandle(HttpHandle handle)
            throws Exception {

            HttpResponse response = handle.getResponse();
            if (response == null) {
                System.out.println("ERROR: response is null in handle!");
            } else {
                System.out.println
                    ("<< Response: " + response.getStatusLine());
                System.out.println
                    (EntityUtils.toString(response.getEntity()));
                /*
                byte[] data = new byte[100];
                int count = response.getEntity().getContent().read(data);
                String s = new String(data, 0, count, "ISO-8859-1");
                System.out.println(s);
                */
            }

            // now that the handle is processed, we can update the counter
            // this should really be done in a finally block:
            synchronized (notification_counter_monitor) {
                notification_counter++;
                notification_counter_monitor.notifyAll();
            }

            handle.close();

        } // processHandle

    } // class ResponseThread


    /**
     * The application thread for problems.
     */
    public final class ProblemThread extends NotifiedServiceThread {

        // default constructor

        /**
         * Called once for each handle routed to this thread.
         *
         * @param handle    the handle
         */
        protected void processHandle(HttpHandle handle) {

            //@@@ There is a lot of test code here which is not relevant
            //@@@ for the example. Remove that as test coverage increases.

            System.out.println("<< Problem: " + handle);

            if (handle.isLinked())
                System.out.println("ERROR: problem handle still linked!");

            // Get the problem from the context. This is application logic:
            // The notification handler has put the problem in the context.
            Throwable pfc = (Throwable) // get problem from context
                handle.getContext().getAttribute(PROBLEM_ATTRIBUTE);

            if (pfc == null)
                System.out.println("ERROR: problem missing from context!");

            // This is the alternative way of getting the problem
            // in the application thread: trigger exception and catch.
            // This is dispatcher logic, not application logic.
            Throwable pfh = null; // problem from handle
            try {
                handle.checkError();
            } catch (Throwable t) { // usually, you'd catch only Exception
                pfh = t;
            }

            if (pfh == null)
                System.out.println("ERROR: problem missing from handle!");

            if (pfc != pfh) // this must be the very same exception
                System.out.println
                    ("ERROR: different problems in context and handle!");

            if (pfc != null) {
                System.out.println("--- problem from context ---");
                pfc.printStackTrace(System.out);
            }
            if ((pfh != null) && (pfh != pfc )) {
                System.out.println("--- problem from handle ---");
                pfh.printStackTrace(System.out);
            }

            // now that the handle is processed, we can update the counter
            // this should really be done in a finally block:
            synchronized (notification_counter_monitor) {
                notification_counter++;
                notification_counter_monitor.notifyAll();
            }

            // handle is not linked anymore, no need to close it

        } // processHandle

    } // class ProblemThread



    /**
     * The notification handler.
     * This class is public only to give you easier access to the JavaDoc.
     */
    public final static class NotifHandlerImpl
        implements HttpNotificationHandler {

        /** The thread to which to route responses. */
        private final NotifiedServiceThread response_target;

        /** The thread to which to route problems. */
        private final NotifiedServiceThread problem_target;


        /**
         * Creates a new notification handler.
         *
         * @param rsp   the routing target for response notifications
         * @param plm   the routing target for problem notifications
         */
        public NotifHandlerImpl(ResponseThread rsp, ProblemThread plm) {

            if (rsp == null)
                throw new IllegalArgumentException
                    ("response handling thread must not be null");
            if (plm == null)
                throw new IllegalArgumentException
                    ("problem handling thread must not be null");

            response_target = rsp;
            problem_target = plm;

        } // constructor NotifHandlerImpl


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

            response_target.queueHandle(handle);

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

            // provide additional information in the context
            handle.getContext().setAttribute(PROBLEM_ATTRIBUTE, problem);

            problem_target.queueHandle(handle);

            return false;

        } // notifyProblem


    } // class NotifHandlerImpl


} // class RoutingAsyncGet
