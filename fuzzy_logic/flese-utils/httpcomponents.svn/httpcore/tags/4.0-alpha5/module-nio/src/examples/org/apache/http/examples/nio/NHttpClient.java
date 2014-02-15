/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-nio/src/examples/org/apache/http/examples/nio/NHttpClient.java $
 * $Revision: 531839 $
 * $Date: 2007-04-24 11:00:05 +0200 (Tue, 24 Apr 2007) $
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
package org.apache.http.examples.nio;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.InetSocketAddress;

import org.apache.http.HttpEntity;
import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.impl.nio.DefaultClientIOEventDispatch;
import org.apache.http.impl.nio.reactor.DefaultConnectingIOReactor;
import org.apache.http.message.BasicHttpRequest;
import org.apache.http.nio.NHttpConnection;
import org.apache.http.nio.protocol.BufferingHttpClientHandler;
import org.apache.http.nio.protocol.EventListener;
import org.apache.http.nio.protocol.HttpRequestExecutionHandler;
import org.apache.http.nio.reactor.ConnectingIOReactor;
import org.apache.http.nio.reactor.IOEventDispatch;
import org.apache.http.nio.reactor.SessionRequest;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.protocol.BasicHttpProcessor;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;
import org.apache.http.protocol.RequestConnControl;
import org.apache.http.protocol.RequestContent;
import org.apache.http.protocol.RequestExpectContinue;
import org.apache.http.protocol.RequestTargetHost;
import org.apache.http.protocol.RequestUserAgent;
import org.apache.http.util.EntityUtils;

public class NHttpClient {

    public static void main(String[] args) throws Exception {
        HttpParams params = new BasicHttpParams(null);
        params
            .setIntParameter(HttpConnectionParams.SO_TIMEOUT, 5000)
            .setIntParameter(HttpConnectionParams.CONNECTION_TIMEOUT, 10000)
            .setIntParameter(HttpConnectionParams.SOCKET_BUFFER_SIZE, 8 * 1024)
            .setBooleanParameter(HttpConnectionParams.STALE_CONNECTION_CHECK, false)
            .setBooleanParameter(HttpConnectionParams.TCP_NODELAY, true)
            .setParameter(HttpProtocolParams.USER_AGENT, "Jakarta-HttpComponents-NIO/1.1");

        final ConnectingIOReactor ioReactor = new DefaultConnectingIOReactor(2, params);

        BasicHttpProcessor httpproc = new BasicHttpProcessor();
        httpproc.addInterceptor(new RequestContent());
        httpproc.addInterceptor(new RequestTargetHost());
        httpproc.addInterceptor(new RequestConnControl());
        httpproc.addInterceptor(new RequestUserAgent());
        httpproc.addInterceptor(new RequestExpectContinue());
        
        BufferingHttpClientHandler handler = new BufferingHttpClientHandler(
                httpproc,
                new MyHttpRequestExecutionHandler(),
                new DefaultConnectionReuseStrategy(),
                params);

        handler.setEventListener(new EventLogger());
        
        final IOEventDispatch ioEventDispatch = new DefaultClientIOEventDispatch(handler, params);
        
        Thread t = new Thread(new Runnable() {
         
            public void run() {
                try {
                    ioReactor.execute(ioEventDispatch);
                } catch (InterruptedIOException ex) {
                    System.err.println("Interrupted");
                } catch (IOException e) {
                    System.err.println("I/O error: " + e.getMessage());
                }
                System.out.println("Shutdown");
            }
            
        });
        t.start();

        SessionRequest[] reqs = new SessionRequest[3];
        reqs[0] = ioReactor.connect(
                new InetSocketAddress("www.yahoo.com", 80), 
                null, 
                new HttpHost("www.yahoo.com"),
                null);
        reqs[1] = ioReactor.connect(
                new InetSocketAddress("www.google.com", 80), 
                null,
                new HttpHost("www.google.ch"),
                null);
        reqs[2] = ioReactor.connect(
                new InetSocketAddress("www.apache.org", 80), 
                null,
                new HttpHost("www.apache.org"),
                null);
        
    }
    
    static class MyHttpRequestExecutionHandler implements HttpRequestExecutionHandler {

        public void initalizeContext(final HttpContext context, final Object attachment) {
            HttpHost targetHost = (HttpHost) attachment;
            context.setAttribute(HttpExecutionContext.HTTP_TARGET_HOST, targetHost);
        }

        public HttpRequest submitRequest(final HttpContext context) {
            HttpHost targetHost = (HttpHost) context.getAttribute(
                    HttpExecutionContext.HTTP_TARGET_HOST);
            Integer countObj = (Integer) context.getAttribute(
                    "request-count");
            int counter = 0; 
            if (countObj != null) {
                counter = countObj.intValue(); 
            }
            counter++;
            context.setAttribute("request-count", new Integer(counter));
            if (counter < 3) {
                System.out.println("--------------");
                System.out.println("Sending request to " + targetHost);
                System.out.println("--------------");
                return new BasicHttpRequest("GET", "/");
            } else {
                // Return null to terminate the connection
                return null;
            }
        }
        
        public void handleResponse(final HttpResponse response, final HttpContext context) {
            HttpEntity entity = response.getEntity();
            try {
                String content = EntityUtils.toString(entity);
                
                System.out.println("--------------");
                System.out.println(response.getStatusLine());
                System.out.println("--------------");
                System.out.println("Document length: " + content.length());
                System.out.println("--------------");
            } catch (IOException ex) {
                System.err.println("I/O error: " + ex.getMessage());
            }
        }
        
    }
    
    static class EventLogger implements EventListener {

        private int openNo = 0;
        private int closedNo = 0;
        
        public void connectionOpen(final NHttpConnection conn) {
            this.openNo++;
            System.out.println("Connection open: " + conn);
        }

        public void connectionTimeout(final NHttpConnection conn) {
            System.out.println("Connection timed out: " + conn);
        }

        public void connectionClosed(final NHttpConnection conn) {
            System.out.println("Connection closed: " + conn);
            this.closedNo++;
            if (this.openNo == this.closedNo) {
                System.exit(0);
            }
        }

        public void fatalIOException(final IOException ex, final NHttpConnection conn) {
            System.err.println("I/O error: " + ex.getMessage());
        }

        public void fatalProtocolException(final HttpException ex, final NHttpConnection conn) {
            System.err.println("HTTP error: " + ex.getMessage());
        }
        
    }
        
}
