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
package org.apache.http.impl.nio.client;

import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Random;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.apache.http.HttpEntity;
import org.apache.http.HttpHost;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.nio.conn.PoolingClientConnectionManager;
import org.apache.http.impl.nio.reactor.DefaultConnectingIOReactor;
import org.apache.http.localserver.ServerTestBase;
import org.apache.http.nio.ContentDecoder;
import org.apache.http.nio.IOControl;
import org.apache.http.nio.client.HttpAsyncClient;
import org.apache.http.nio.conn.scheme.Scheme;
import org.apache.http.nio.conn.scheme.SchemeRegistry;
import org.apache.http.nio.entity.NByteArrayEntity;
import org.apache.http.nio.reactor.ConnectingIOReactor;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.util.EntityUtils;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TestHttpAsync extends ServerTestBase {

    private HttpHost target;
    private PoolingClientConnectionManager sessionManager;
    private HttpAsyncClient httpclient;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        this.localServer.registerDefaultHandlers();
        int port = this.localServer.getServiceAddress().getPort();
        this.target = new HttpHost("localhost", port);

        ConnectingIOReactor ioReactor = new DefaultConnectingIOReactor(2, new BasicHttpParams());
        SchemeRegistry schemeRegistry = new SchemeRegistry();
        schemeRegistry.register(new Scheme("http", 80, null));
        this.sessionManager = new PoolingClientConnectionManager(ioReactor, schemeRegistry);
        this.httpclient = new DefaultHttpAsyncClient(ioReactor, this.sessionManager);
    }

    @After
    public void tearDown() throws Exception {
        this.httpclient.shutdown();
        super.tearDown();
    }

    @Test
    public void testSingleGet() throws Exception {
        this.httpclient.start();
        HttpGet httpget = new HttpGet("/random/2048");
        Future<HttpResponse> future = this.httpclient.execute(this.target, httpget, null);
        HttpResponse response = future.get();
        Assert.assertNotNull(response);
        Assert.assertEquals(200, response.getStatusLine().getStatusCode());
    }

    @Test
    public void testSinglePost() throws Exception {
        byte[] b1 = new byte[1024];
        Random rnd = new Random(System.currentTimeMillis());
        rnd.nextBytes(b1);

        this.httpclient.start();

        HttpPost httppost = new HttpPost("/echo/stuff");
        httppost.setEntity(new NByteArrayEntity(b1));

        Future<HttpResponse> future = this.httpclient.execute(this.target, httppost, null);
        HttpResponse response = future.get();
        Assert.assertNotNull(response);
        Assert.assertEquals(200, response.getStatusLine().getStatusCode());
        HttpEntity entity = response.getEntity();
        Assert.assertNotNull(entity);
        byte[] b2 = EntityUtils.toByteArray(entity);
        Assert.assertArrayEquals(b1, b2);
    }

    @Test
    public void testMultiplePostsOverMultipleConnections() throws Exception {
        byte[] b1 = new byte[1024];
        Random rnd = new Random(System.currentTimeMillis());
        rnd.nextBytes(b1);

        int reqCount = 20;

        this.sessionManager.setDefaultMaxPerHost(reqCount);
        this.sessionManager.setTotalMax(100);
        this.httpclient.start();

        Queue<Future<HttpResponse>> queue = new LinkedList<Future<HttpResponse>>();

        for (int i = 0; i < reqCount; i++) {
            HttpPost httppost = new HttpPost("/echo/stuff");
            httppost.setEntity(new NByteArrayEntity(b1));
            queue.add(this.httpclient.execute(this.target, httppost, null));
        }

        while (!queue.isEmpty()) {
            Future<HttpResponse> future = queue.remove();
            HttpResponse response = future.get();
            Assert.assertNotNull(response);
            Assert.assertEquals(200, response.getStatusLine().getStatusCode());
            HttpEntity entity = response.getEntity();
            Assert.assertNotNull(entity);
            byte[] b2 = EntityUtils.toByteArray(entity);
            Assert.assertArrayEquals(b1, b2);
        }
    }

    @Test
    public void testMultiplePostsOverSingleConnection() throws Exception {
        byte[] b1 = new byte[1024];
        Random rnd = new Random(System.currentTimeMillis());
        rnd.nextBytes(b1);

        int reqCount = 20;

        this.sessionManager.setDefaultMaxPerHost(1);
        this.sessionManager.setTotalMax(100);
        this.httpclient.start();

        Queue<Future<HttpResponse>> queue = new LinkedList<Future<HttpResponse>>();

        for (int i = 0; i < reqCount; i++) {
            HttpPost httppost = new HttpPost("/echo/stuff");
            httppost.setEntity(new NByteArrayEntity(b1));
            queue.add(this.httpclient.execute(this.target, httppost, null));
        }

        while (!queue.isEmpty()) {
            Future<HttpResponse> future = queue.remove();
            HttpResponse response = future.get();
            Assert.assertNotNull(response);
            Assert.assertEquals(200, response.getStatusLine().getStatusCode());
            HttpEntity entity = response.getEntity();
            Assert.assertNotNull(entity);
            byte[] b2 = EntityUtils.toByteArray(entity);
            Assert.assertArrayEquals(b1, b2);
        }
    }

    @Test
    public void testRequestFailure() throws Exception {
        this.httpclient.start();

        HttpGet httpget = new HttpGet("/random/2048");
        BasicHttpAsyncRequestProducer requestProducer = new BasicHttpAsyncRequestProducer(this.target, httpget) ;
        BasicHttpAsyncResponseConsumer responseConsumer = new BasicHttpAsyncResponseConsumer() {

            @Override
            public void consumeContent(final ContentDecoder decoder, final IOControl ioctrl)
                    throws IOException {
                throw new IOException("Kaboom");
            }

        };
        Future<HttpResponse> future = this.httpclient.execute(requestProducer, responseConsumer, null);
        try {
            future.get();
            Assert.fail("ExecutionException expected");
        } catch (ExecutionException ex) {
            Throwable t = ex.getCause();
            Assert.assertNotNull(t);
            Assert.assertTrue(t instanceof IOException);
            Assert.assertEquals("Kaboom", t.getMessage());
        }
    }

}
