/*
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
package org.apache.http.nio.client.integration;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.apache.http.HttpAsyncTestBase;
import org.apache.http.HttpConnection;
import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.concurrent.FutureCallback;
import org.apache.http.entity.ContentType;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.impl.DefaultHttpResponseFactory;
import org.apache.http.impl.nio.DefaultNHttpServerConnection;
import org.apache.http.impl.nio.DefaultNHttpServerConnectionFactory;
import org.apache.http.nio.ContentEncoder;
import org.apache.http.nio.IOControl;
import org.apache.http.nio.NHttpConnectionFactory;
import org.apache.http.nio.entity.NStringEntity;
import org.apache.http.nio.protocol.BasicAsyncRequestConsumer;
import org.apache.http.nio.protocol.BasicAsyncResponseProducer;
import org.apache.http.nio.protocol.HttpAsyncExchange;
import org.apache.http.nio.protocol.HttpAsyncExpectationVerifier;
import org.apache.http.nio.protocol.HttpAsyncRequestConsumer;
import org.apache.http.nio.protocol.HttpAsyncRequestHandler;
import org.apache.http.nio.protocol.HttpAsyncRequestHandlerRegistry;
import org.apache.http.nio.protocol.HttpAsyncRequestHandlerResolver;
import org.apache.http.nio.protocol.HttpAsyncService;
import org.apache.http.nio.reactor.IOReactorStatus;
import org.apache.http.nio.reactor.ListenerEndpoint;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.ExecutionContext;
import org.apache.http.protocol.HttpContext;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TestHttpAsyncPrematureTermination extends HttpAsyncTestBase {

    @Before
    public void setUp() throws Exception {
        initServer();
        initClient();
    }

    @After
    public void tearDown() throws Exception {
        shutDownClient();
        shutDownServer();
    }

    @Override
    protected NHttpConnectionFactory<DefaultNHttpServerConnection> createServerConnectionFactory(
            final HttpParams params) throws Exception {
        return new DefaultNHttpServerConnectionFactory(params);
    }

    @Override
    protected String getSchemeName() {
        return "http";
    }

    private HttpHost start(
            final HttpAsyncRequestHandlerResolver requestHandlerResolver,
            final HttpAsyncExpectationVerifier expectationVerifier) throws Exception {
        HttpAsyncService serviceHandler = new HttpAsyncService(
                this.serverHttpProc,
                new DefaultConnectionReuseStrategy(),
                new DefaultHttpResponseFactory(),
                requestHandlerResolver,
                expectationVerifier,
                this.serverParams);
        this.server.start(serviceHandler);
        this.httpclient.start();

        ListenerEndpoint endpoint = this.server.getListenerEndpoint();
        endpoint.waitFor();

        Assert.assertEquals("Test server status", IOReactorStatus.ACTIVE, this.server.getStatus());
        InetSocketAddress address = (InetSocketAddress) endpoint.getAddress();
        HttpHost target = new HttpHost("localhost", address.getPort(), getSchemeName());
        return target;
    }

    @Test
    public void testConnectionTerminatedProcessingRequest() throws Exception {
        HttpAsyncRequestHandlerRegistry registry = new HttpAsyncRequestHandlerRegistry();
        registry.register("*", new HttpAsyncRequestHandler<HttpRequest>() {

            public HttpAsyncRequestConsumer<HttpRequest> processRequest(
                    final HttpRequest request,
                    final HttpContext context) throws HttpException, IOException {
                HttpConnection conn = (HttpConnection) context.getAttribute(
                        ExecutionContext.HTTP_CONNECTION);
                conn.shutdown();
                return new BasicAsyncRequestConsumer();
            }

            public void handle(
                    final HttpRequest request,
                    final HttpAsyncExchange httpExchange,
                    final HttpContext context) throws HttpException, IOException {
                HttpResponse response = httpExchange.getResponse();
                response.setEntity(new NStringEntity("all is well", ContentType.TEXT_PLAIN));
                httpExchange.submitResponse();
            }

        });
        HttpHost target = start(registry, null);
        HttpGet httpget = new HttpGet("/");

        final CountDownLatch latch = new CountDownLatch(1);

        FutureCallback<HttpResponse> callback = new FutureCallback<HttpResponse>() {

            public void cancelled() {
                latch.countDown();
            }

            public void failed(final Exception ex) {
                latch.countDown();
            }

            public void completed(final HttpResponse response) {
                Assert.fail();
            }

        };

        this.httpclient.execute(target, httpget, callback);
        Assert.assertTrue(latch.await(5, TimeUnit.SECONDS));
    }

    @Test
    public void testConnectionTerminatedHandlingRequest() throws Exception {
        HttpAsyncRequestHandlerRegistry registry = new HttpAsyncRequestHandlerRegistry();
        registry.register("*", new HttpAsyncRequestHandler<HttpRequest>() {

            public HttpAsyncRequestConsumer<HttpRequest> processRequest(
                    final HttpRequest request,
                    final HttpContext context) throws HttpException, IOException {
                return new BasicAsyncRequestConsumer();
            }

            public void handle(
                    final HttpRequest request,
                    final HttpAsyncExchange httpExchange,
                    final HttpContext context) throws HttpException, IOException {
                HttpConnection conn = (HttpConnection) context.getAttribute(
                        ExecutionContext.HTTP_CONNECTION);
                conn.shutdown();
                HttpResponse response = httpExchange.getResponse();
                response.setEntity(new NStringEntity("all is well", ContentType.TEXT_PLAIN));
                httpExchange.submitResponse();
            }

        });
        HttpHost target = start(registry, null);
        HttpGet httpget = new HttpGet("/");

        final CountDownLatch latch = new CountDownLatch(1);

        FutureCallback<HttpResponse> callback = new FutureCallback<HttpResponse>() {

            public void cancelled() {
                latch.countDown();
            }

            public void failed(final Exception ex) {
                latch.countDown();
            }

            public void completed(final HttpResponse response) {
                Assert.fail();
            }

        };

        this.httpclient.execute(target, httpget, callback);
        Assert.assertTrue(latch.await(5, TimeUnit.SECONDS));
    }

    @Test
    public void testConnectionTerminatedSendingResponse() throws Exception {
        HttpAsyncRequestHandlerRegistry registry = new HttpAsyncRequestHandlerRegistry();
        registry.register("*", new HttpAsyncRequestHandler<HttpRequest>() {

            public HttpAsyncRequestConsumer<HttpRequest> processRequest(
                    final HttpRequest request,
                    final HttpContext context) throws HttpException, IOException {
                return new BasicAsyncRequestConsumer();
            }

            public void handle(
                    final HttpRequest request,
                    final HttpAsyncExchange httpExchange,
                    final HttpContext context) throws HttpException, IOException {
                HttpResponse response = httpExchange.getResponse();
                response.setEntity(new NStringEntity("all is well", ContentType.TEXT_PLAIN));
                httpExchange.submitResponse(new BasicAsyncResponseProducer(response) {

                    @Override
                    public synchronized void produceContent(
                            final ContentEncoder encoder,
                            final IOControl ioctrl) throws IOException {
                        ioctrl.shutdown();
                    }

                });
            }

        });
        HttpHost target = start(registry, null);
        HttpGet httpget = new HttpGet("/");

        final CountDownLatch latch = new CountDownLatch(1);

        FutureCallback<HttpResponse> callback = new FutureCallback<HttpResponse>() {

            public void cancelled() {
                latch.countDown();
            }

            public void failed(final Exception ex) {
                latch.countDown();
            }

            public void completed(final HttpResponse response) {
                Assert.fail();
            }

        };

        this.httpclient.execute(target, httpget, callback);
        Assert.assertTrue(latch.await(5, TimeUnit.SECONDS));
    }

}
