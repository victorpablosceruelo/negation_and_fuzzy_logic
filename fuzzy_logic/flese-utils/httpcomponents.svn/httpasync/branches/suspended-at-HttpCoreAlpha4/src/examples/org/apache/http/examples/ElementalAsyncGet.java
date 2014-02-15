/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/examples/org/apache/http/examples/ElementalAsyncGet.java $
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


import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpHost;
import org.apache.http.HttpResponse;
import org.apache.http.HttpVersion;
import org.apache.http.async.AsyncHttpProcessor;
import org.apache.http.async.HttpAsyncClientConnection;
import org.apache.http.async.HttpDispatcher;
import org.apache.http.async.HttpHandle;
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
 * Example for using an asynchronous {@link HttpDispatcher dispatcher}.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 505896 $
 */
public class ElementalAsyncGet {

    /**
     * Main entry point to this example.
     *
     * @param args        command line arguments
     */
    public static void main(String[] args) throws Exception {
        
        HttpDispatcher dispatcher = createDispatcher();
        System.out.println("dispatcher " + dispatcher + "\n");

        String[] targets = args;
        if ((targets == null) || (targets.length < 1)) {
            targets = new String[] {
                "/",
                "/servlets-examples/servlet/RequestInfoExample", 
                "/somewhere%20in%20pampa"
            };
        }

        HttpHost     host    = new HttpHost("localhost", 8080);
        HttpHandle[] handles = new HttpHandle[targets.length];

        for (int i = 0; i < targets.length; i++) {

            HttpGet request = new HttpGet(targets[i]);
            System.out.println(">> Request URI: " +
                               request.getRequestLine().getUri());
            handles[i] = dispatcher.sendRequest(request, host, null);
            System.out.println(">> Handle: " + handles[i]);
            System.out.println("==============");

        } // for targets

        System.out.println("\ndispatcher " + dispatcher + "\n");

        // now pick up the responses
        for (int i = 0; i < targets.length; i++) {

            if (handles[i] == null) {
                System.out.println("<< No handle for " + targets[i]);

            } else {
                // Blocking on a request after dispatching several of them
                // is generally dangerous and may lead to deadlocks. With
                // the simple dispatcher used here, blocking in order of
                // sending is safe. Blocking in any other order is never safe.

                HttpResponse response = handles[i].awaitResponse();
                if (response != null) {
                    System.out.println
                        ("<< Response: " + response.getStatusLine());
                    System.out.println
                        (EntityUtils.toString(response.getEntity()));
                    /*
                    byte [] data = new byte[100];
                    int count = response.getEntity().getContent().read(data);
                    String s = new String(data, 0, count, "ISO-8859-1");
                    System.out.println(s);
                    */
                } else {
                    System.out.println("<< Response: null");
                    try {
                        handles[i].checkError();
                    } catch (Exception x) {
                        x.printStackTrace(System.out);
                    }
                }
                System.out.println("--------------");
                handles[i].close();
                System.out.println(dispatcher);
                System.out.println("==============");
            }
        } // for handles

    } // main


    /**
     * Instantiates a dispatcher.
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


} // class ElementalAsyncGet
