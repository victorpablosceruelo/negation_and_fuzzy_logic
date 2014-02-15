/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-nio/src/test/java/org/apache/http/nio/protocol/TestNIOHttp.java $
 * $Revision: 516114 $
 * $Date: 2007-03-08 18:35:33 +0100 (Thu, 08 Mar 2007) $
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

package org.apache.http.nio.protocol;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.entity.StringEntity;
import org.apache.http.message.HttpGet;
import org.apache.http.message.HttpPost;
import org.apache.http.nio.NHttpConnection;
import org.apache.http.nio.mockup.TestHttpClient;
import org.apache.http.nio.mockup.TestHttpServer;
import org.apache.http.nio.protocol.HttpRequestExecutionHandler;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;
import org.apache.http.protocol.HttpExpectationVerifier;
import org.apache.http.protocol.HttpRequestHandler;
import org.apache.http.util.EncodingUtils;
import org.apache.http.util.EntityUtils;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * HttpCore NIO integration tests.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Id: TestNIOHttp.java 516114 2007-03-08 17:35:33Z olegk $
 */
public class TestNIOHttp extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestNIOHttp(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestNIOHttp.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestNIOHttp.class);
    }

    private TestHttpServer server;
    private TestHttpClient client;
    
    protected void setUp() throws Exception {
        this.server = new TestHttpServer();
        this.client = new TestHttpClient();
    }

    protected void tearDown() throws Exception {
        this.server.shutdown();
        this.client.shutdown();
    }

    /**
     * This test case executes a series of simple (non-pipelined) GET requests 
     * over multiple connections. 
     */
    public void testSimpleHttpGets() throws Exception {
        
        final int connNo = 3;
        final int reqNo = 20;
        
        Random rnd = new Random();
        
        // Prepare some random data
        final List testData = new ArrayList(reqNo);
        for (int i = 0; i < reqNo; i++) {
            int size = rnd.nextInt(5000);
            byte[] data = new byte[size];
            rnd.nextBytes(data);
            testData.add(data);
        }
        
        List[] responseData = new List[connNo];
        for (int i = 0; i < responseData.length; i++) {
            responseData[i] = new ArrayList();
        }
        
        // Initialize the server-side request handler
        this.server.registerHandler("*", new HttpRequestHandler() {

            public void handle(
                    final HttpRequest request, 
                    final HttpResponse response, 
                    final HttpContext context) throws HttpException, IOException {
                
                String s = request.getRequestLine().getUri();
                URI uri;
                try {
                    uri = new URI(s);
                } catch (URISyntaxException ex) {
                    throw new HttpException("Invalid request URI: " + s);
                }
                int index = Integer.parseInt(uri.getQuery());
                byte[] data = (byte []) testData.get(index);
                ByteArrayEntity entity = new ByteArrayEntity(data); 
                response.setEntity(entity);
            }
            
        });
        
        // Initialize the client side request executor
        this.client.setHttpRequestExecutionHandler(new HttpRequestExecutionHandler() {

            public void initalizeContext(final HttpContext context, final Object attachment) {
                context.setAttribute("LIST", (List) attachment);
                context.setAttribute("REQ-COUNT", new Integer(0));
                context.setAttribute("RES-COUNT", new Integer(0));
            }

            public HttpRequest submitRequest(final HttpContext context) {
                int i = ((Integer) context.getAttribute("REQ-COUNT")).intValue();
                HttpGet get = null;
                if (i < reqNo) {
                    get = new HttpGet("/?" + i);
                    context.setAttribute("REQ-COUNT", new Integer(i + 1));
                }
                return get;
            }
            
            public void handleResponse(final HttpResponse response, final HttpContext context) {
                NHttpConnection conn = (NHttpConnection) context.getAttribute(
                        HttpExecutionContext.HTTP_CONNECTION);
                
                List list = (List) context.getAttribute("LIST");
                int i = ((Integer) context.getAttribute("RES-COUNT")).intValue();
                i++;
                context.setAttribute("RES-COUNT", new Integer(i));

                try {
                    HttpEntity entity = response.getEntity();
                    byte[] data = EntityUtils.toByteArray(entity);
                    list.add(data);
                } catch (IOException ex) {
                    fail(ex.getMessage());
                }

                if (i < reqNo) {
                    conn.requestInput();
                } else {
                    try {
                        conn.close();
                    } catch (IOException ex) {
                        fail(ex.getMessage());
                    }
                }
            }
            
        });
        
        this.server.start();
        this.client.start();
        
        InetSocketAddress serverAddress = (InetSocketAddress) this.server.getSocketAddress();
        
        for (int i = 0; i < responseData.length; i++) {
            this.client.openConnection(
                    new InetSocketAddress("localhost", serverAddress.getPort()), 
                    responseData[i]);
        }
     
        this.client.await(connNo, 1000);
        assertEquals(connNo, this.client.getConnCount());
        
        this.client.shutdown();
        this.server.shutdown();

        for (int c = 0; c < responseData.length; c++) {
            List receivedPackets = responseData[c];
            List expectedPackets = testData;
            assertEquals(expectedPackets.size(), receivedPackets.size());
            for (int p = 0; p < testData.size(); p++) {
                byte[] expected = (byte[]) testData.get(p);
                byte[] received = (byte[]) receivedPackets.get(p);
                
                assertEquals(expected.length, received.length);
                for (int i = 0; i < expected.length; i++) {
                    assertEquals(expected[i], received[i]);
                }
            }
        }
        
    }

    /**
     * This test case executes a series of simple (non-pipelined) POST requests 
     * with content length delimited content over multiple connections. 
     */
    public void testSimpleHttpPostsWithContentLength() throws Exception {
        
        final int connNo = 3;
        final int reqNo = 20;
        
        Random rnd = new Random();
        
        // Prepare some random data
        final List testData = new ArrayList(reqNo);
        for (int i = 0; i < reqNo; i++) {
            int size = rnd.nextInt(5000);
            byte[] data = new byte[size];
            rnd.nextBytes(data);
            testData.add(data);
        }
        
        List[] responseData = new List[connNo];
        for (int i = 0; i < responseData.length; i++) {
            responseData[i] = new ArrayList();
        }
        
        // Initialize the server-side request handler
        this.server.registerHandler("*", new HttpRequestHandler() {

            public void handle(
                    final HttpRequest request, 
                    final HttpResponse response, 
                    final HttpContext context) throws HttpException, IOException {
                
                if (request instanceof HttpEntityEnclosingRequest) {
                    HttpEntity incoming = ((HttpEntityEnclosingRequest) request).getEntity();
                    byte[] data = EntityUtils.toByteArray(incoming);
                    
                    ByteArrayEntity outgoing = new ByteArrayEntity(data);
                    outgoing.setChunked(false);
                    response.setEntity(outgoing);
                } else {
                    StringEntity outgoing = new StringEntity("No content"); 
                    response.setEntity(outgoing);
                }
            }
            
        });
        
        // Initialize the client side request executor
        this.client.setHttpRequestExecutionHandler(new HttpRequestExecutionHandler() {

            public void initalizeContext(final HttpContext context, final Object attachment) {
                context.setAttribute("LIST", (List) attachment);
                context.setAttribute("REQ-COUNT", new Integer(0));
                context.setAttribute("RES-COUNT", new Integer(0));
            }

            public HttpRequest submitRequest(final HttpContext context) {
                int i = ((Integer) context.getAttribute("REQ-COUNT")).intValue();
                HttpPost post = null;
                if (i < reqNo) {
                    post = new HttpPost("/?" + i);
                    byte[] data = (byte[]) testData.get(i);
                    ByteArrayEntity outgoing = new ByteArrayEntity(data);
                    post.setEntity(outgoing);
                    
                    context.setAttribute("REQ-COUNT", new Integer(i + 1));
                }
                return post;
            }
            
            public void handleResponse(final HttpResponse response, final HttpContext context) {
                NHttpConnection conn = (NHttpConnection) context.getAttribute(
                        HttpExecutionContext.HTTP_CONNECTION);
                
                List list = (List) context.getAttribute("LIST");
                int i = ((Integer) context.getAttribute("RES-COUNT")).intValue();
                i++;
                context.setAttribute("RES-COUNT", new Integer(i));

                try {
                    HttpEntity entity = response.getEntity();
                    byte[] data = EntityUtils.toByteArray(entity);
                    list.add(data);
                } catch (IOException ex) {
                    fail(ex.getMessage());
                }

                if (i < reqNo) {
                    conn.requestInput();
                } else {
                    try {
                        conn.close();
                    } catch (IOException ex) {
                        fail(ex.getMessage());
                    }
                }
            }
            
        });
        
        this.server.start();
        this.client.start();
        
        InetSocketAddress serverAddress = (InetSocketAddress) this.server.getSocketAddress();
        
        for (int i = 0; i < responseData.length; i++) {
            this.client.openConnection(
                    new InetSocketAddress("localhost", serverAddress.getPort()), 
                    responseData[i]);
        }
     
        this.client.await(connNo, 1000);
        assertEquals(connNo, this.client.getConnCount());
        
        this.client.shutdown();
        this.server.shutdown();

        for (int c = 0; c < responseData.length; c++) {
            List receivedPackets = responseData[c];
            List expectedPackets = testData;
            assertEquals(expectedPackets.size(), receivedPackets.size());
            for (int p = 0; p < testData.size(); p++) {
                byte[] expected = (byte[]) testData.get(p);
                byte[] received = (byte[]) receivedPackets.get(p);
                
                assertEquals(expected.length, received.length);
                for (int i = 0; i < expected.length; i++) {
                    assertEquals(expected[i], received[i]);
                }
            }
        }
        
    }

    /**
     * This test case executes a series of simple (non-pipelined) POST requests 
     * with chunk coded content content over multiple connections. 
     */
    public void testSimpleHttpPostsChunked() throws Exception {
        
        final int connNo = 3;
        final int reqNo = 20;
        
        Random rnd = new Random();
        
        // Prepare some random data
        final List testData = new ArrayList(reqNo);
        for (int i = 0; i < reqNo; i++) {
            int size = rnd.nextInt(20000);
            byte[] data = new byte[size];
            rnd.nextBytes(data);
            testData.add(data);
        }
        
        List[] responseData = new List[connNo];
        for (int i = 0; i < responseData.length; i++) {
            responseData[i] = new ArrayList();
        }
        
        // Initialize the server-side request handler
        this.server.registerHandler("*", new HttpRequestHandler() {

            public void handle(
                    final HttpRequest request, 
                    final HttpResponse response, 
                    final HttpContext context) throws HttpException, IOException {
                
                if (request instanceof HttpEntityEnclosingRequest) {
                    HttpEntity incoming = ((HttpEntityEnclosingRequest) request).getEntity();
                    byte[] data = EntityUtils.toByteArray(incoming);
                    ByteArrayEntity outgoing = new ByteArrayEntity(data);
                    outgoing.setChunked(true);
                    response.setEntity(outgoing);
                } else {
                    StringEntity outgoing = new StringEntity("No content"); 
                    response.setEntity(outgoing);
                }
            }
            
        });
        
        // Initialize the client side request executor
        this.client.setHttpRequestExecutionHandler(new HttpRequestExecutionHandler() {

            public void initalizeContext(final HttpContext context, final Object attachment) {
                context.setAttribute("LIST", (List) attachment);
                context.setAttribute("REQ-COUNT", new Integer(0));
                context.setAttribute("RES-COUNT", new Integer(0));
            }

            public HttpRequest submitRequest(final HttpContext context) {
                int i = ((Integer) context.getAttribute("REQ-COUNT")).intValue();
                HttpPost post = null;
                if (i < reqNo) {
                    post = new HttpPost("/?" + i);
                    byte[] data = (byte[]) testData.get(i);
                    ByteArrayEntity outgoing = new ByteArrayEntity(data);
                    outgoing.setChunked(true);
                    post.setEntity(outgoing);
                    
                    context.setAttribute("REQ-COUNT", new Integer(i + 1));
                }
                return post;
            }
            
            public void handleResponse(final HttpResponse response, final HttpContext context) {
                NHttpConnection conn = (NHttpConnection) context.getAttribute(
                        HttpExecutionContext.HTTP_CONNECTION);
                
                List list = (List) context.getAttribute("LIST");
                int i = ((Integer) context.getAttribute("RES-COUNT")).intValue();
                i++;
                context.setAttribute("RES-COUNT", new Integer(i));
                
                try {
                    HttpEntity entity = response.getEntity();
                    byte[] data = EntityUtils.toByteArray(entity);
                    list.add(data);
                } catch (IOException ex) {
                    fail(ex.getMessage());
                }

                if (i < reqNo) {
                    conn.requestInput();
                } else {
                    try {
                        conn.close();
                    } catch (IOException ex) {
                        fail(ex.getMessage());
                    }
                }
            }
            
        });
        
        this.server.start();
        this.client.start();
        
        InetSocketAddress serverAddress = (InetSocketAddress) this.server.getSocketAddress();
        
        for (int i = 0; i < responseData.length; i++) {
            this.client.openConnection(
                    new InetSocketAddress("localhost", serverAddress.getPort()), 
                    responseData[i]);
        }
     
        this.client.await(connNo, 1000);
        assertEquals(connNo, this.client.getConnCount());
        
        this.client.shutdown();
        this.server.shutdown();

        for (int c = 0; c < responseData.length; c++) {
            List receivedPackets = responseData[c];
            List expectedPackets = testData;
            assertEquals(expectedPackets.size(), receivedPackets.size());
            for (int p = 0; p < testData.size(); p++) {
                byte[] expected = (byte[]) testData.get(p);
                byte[] received = (byte[]) receivedPackets.get(p);
                
                assertEquals(expected.length, received.length);
                for (int i = 0; i < expected.length; i++) {
                    assertEquals(expected[i], received[i]);
                }
            }
        }
        
    }

    /**
     * This test case executes a series of simple (non-pipelined) HTTP/1.0 
     * POST requests over multiple persistent connections. 
     */
    public void testSimpleHttpPostsHTTP10() throws Exception {
        
        final int connNo = 3;
        final int reqNo = 20;
        
        Random rnd = new Random();
        
        // Prepare some random data
        final List testData = new ArrayList(reqNo);
        for (int i = 0; i < reqNo; i++) {
            int size = rnd.nextInt(5000);
            byte[] data = new byte[size];
            rnd.nextBytes(data);
            testData.add(data);
        }
        
        List[] responseData = new List[connNo];
        for (int i = 0; i < responseData.length; i++) {
            responseData[i] = new ArrayList();
        }
        
        // Initialize the server-side request handler
        this.server.registerHandler("*", new HttpRequestHandler() {
            

            public void handle(
                    final HttpRequest request, 
                    final HttpResponse response, 
                    final HttpContext context) throws HttpException, IOException {
                
                if (request instanceof HttpEntityEnclosingRequest) {
                    HttpEntity incoming = ((HttpEntityEnclosingRequest) request).getEntity();
                    byte[] data = EntityUtils.toByteArray(incoming);
                    
                    ByteArrayEntity outgoing = new ByteArrayEntity(data);
                    outgoing.setChunked(false);
                    response.setEntity(outgoing);
                } else {
                    StringEntity outgoing = new StringEntity("No content"); 
                    response.setEntity(outgoing);
                }
            }
            
        });
        
        // Initialize the client side request executor
        // Set protocol level to HTTP/1.0
        this.client.getParams().setParameter(
                HttpProtocolParams.PROTOCOL_VERSION, HttpVersion.HTTP_1_0);
        this.client.setHttpRequestExecutionHandler(new HttpRequestExecutionHandler() {

            public void initalizeContext(final HttpContext context, final Object attachment) {
                context.setAttribute("LIST", (List) attachment);
                context.setAttribute("REQ-COUNT", new Integer(0));
                context.setAttribute("RES-COUNT", new Integer(0));
            }

            public HttpRequest submitRequest(final HttpContext context) {
                int i = ((Integer) context.getAttribute("REQ-COUNT")).intValue();
                HttpPost post = null;
                if (i < reqNo) {
                    post = new HttpPost("/?" + i);
                    byte[] data = (byte[]) testData.get(i);
                    ByteArrayEntity outgoing = new ByteArrayEntity(data);
                    post.setEntity(outgoing);
                    
                    context.setAttribute("REQ-COUNT", new Integer(i + 1));
                }
                return post;
            }
            
            public void handleResponse(final HttpResponse response, final HttpContext context) {
                NHttpConnection conn = (NHttpConnection) context.getAttribute(
                        HttpExecutionContext.HTTP_CONNECTION);

                List list = (List) context.getAttribute("LIST");
                int i = ((Integer) context.getAttribute("RES-COUNT")).intValue();
                i++;
                context.setAttribute("RES-COUNT", new Integer(i));

                try {
                    HttpEntity entity = response.getEntity();
                    byte[] data = EntityUtils.toByteArray(entity);
                    list.add(data);
                } catch (IOException ex) {
                    fail(ex.getMessage());
                }

                if (i < reqNo) {
                    conn.requestInput();
                } else {
                    try {
                        conn.close();
                    } catch (IOException ex) {
                        fail(ex.getMessage());
                    }
                }
            }
            
        });
        
        this.server.start();
        this.client.start();
        
        InetSocketAddress serverAddress = (InetSocketAddress) this.server.getSocketAddress();
        
        for (int i = 0; i < responseData.length; i++) {
            this.client.openConnection(
                    new InetSocketAddress("localhost", serverAddress.getPort()), 
                    responseData[i]);
        }
     
        this.client.await(connNo, 1000);
        assertEquals(connNo, this.client.getConnCount());
        
        this.client.shutdown();
        this.server.shutdown();

        for (int c = 0; c < responseData.length; c++) {
            List receivedPackets = responseData[c];
            List expectedPackets = testData;
            assertEquals(expectedPackets.size(), receivedPackets.size());
            for (int p = 0; p < testData.size(); p++) {
                byte[] expected = (byte[]) testData.get(p);
                byte[] received = (byte[]) receivedPackets.get(p);
                
                assertEquals(expected.length, received.length);
                for (int i = 0; i < expected.length; i++) {
                    assertEquals(expected[i], received[i]);
                }
            }
        }
        
    }

    /**
     * This test case executes a series of simple (non-pipelined) POST requests 
     * over multiple connections using the 'expect: continue' handshake. 
     */
    public void testHttpPostsWithExpectContinue() throws Exception {
        
        final int connNo = 3;
        final int reqNo = 20;
        
        Random rnd = new Random();
        
        // Prepare some random data
        final List testData = new ArrayList(reqNo);
        for (int i = 0; i < reqNo; i++) {
            int size = rnd.nextInt(20000);
            byte[] data = new byte[size];
            rnd.nextBytes(data);
            testData.add(data);
        }
        
        List[] responseData = new List[connNo];
        for (int i = 0; i < responseData.length; i++) {
            responseData[i] = new ArrayList();
        }
        
        // Initialize the server-side request handler
        this.server.registerHandler("*", new HttpRequestHandler() {

            public void handle(
                    final HttpRequest request, 
                    final HttpResponse response, 
                    final HttpContext context) throws HttpException, IOException {
                
                if (request instanceof HttpEntityEnclosingRequest) {
                    HttpEntity incoming = ((HttpEntityEnclosingRequest) request).getEntity();
                    byte[] data = EntityUtils.toByteArray(incoming);
                    ByteArrayEntity outgoing = new ByteArrayEntity(data);
                    outgoing.setChunked(true);
                    response.setEntity(outgoing);
                } else {
                    StringEntity outgoing = new StringEntity("No content"); 
                    response.setEntity(outgoing);
                }
            }
            
        });

        // Activate 'expect: continue' handshake
        this.client.getParams().setBooleanParameter(HttpProtocolParams.USE_EXPECT_CONTINUE, true);
        // Initialize the client side request executor
        this.client.setHttpRequestExecutionHandler(new HttpRequestExecutionHandler() {

            public void initalizeContext(final HttpContext context, final Object attachment) {
                context.setAttribute("LIST", (List) attachment);
                context.setAttribute("REQ-COUNT", new Integer(0));
                context.setAttribute("RES-COUNT", new Integer(0));
            }

            public HttpRequest submitRequest(final HttpContext context) {
                int i = ((Integer) context.getAttribute("REQ-COUNT")).intValue();
                HttpPost post = null;
                if (i < reqNo) {
                    post = new HttpPost("/?" + i);
                    byte[] data = (byte[]) testData.get(i);
                    ByteArrayEntity outgoing = new ByteArrayEntity(data);
                    outgoing.setChunked(true);
                    post.setEntity(outgoing);
                    
                    context.setAttribute("REQ-COUNT", new Integer(i + 1));
                }
                return post;
            }
            
            public void handleResponse(final HttpResponse response, final HttpContext context) {
                NHttpConnection conn = (NHttpConnection) context.getAttribute(
                        HttpExecutionContext.HTTP_CONNECTION);
                
                List list = (List) context.getAttribute("LIST");
                int i = ((Integer) context.getAttribute("RES-COUNT")).intValue();
                i++;
                context.setAttribute("RES-COUNT", new Integer(i));
                
                try {
                    HttpEntity entity = response.getEntity();
                    byte[] data = EntityUtils.toByteArray(entity);
                    list.add(data);
                } catch (IOException ex) {
                    fail(ex.getMessage());
                }

                if (i < reqNo) {
                    conn.requestInput();
                } else {
                    try {
                        conn.close();
                    } catch (IOException ex) {
                        fail(ex.getMessage());
                    }
                }
            }
            
        });
        
        this.server.start();
        this.client.start();
        
        InetSocketAddress serverAddress = (InetSocketAddress) this.server.getSocketAddress();
        
        for (int i = 0; i < responseData.length; i++) {
            this.client.openConnection(
                    new InetSocketAddress("localhost", serverAddress.getPort()), 
                    responseData[i]);
        }
     
        this.client.await(connNo, 1000);
        assertEquals(connNo, this.client.getConnCount());
        
        this.client.shutdown();
        this.server.shutdown();

        for (int c = 0; c < responseData.length; c++) {
            List receivedPackets = responseData[c];
            List expectedPackets = testData;
            assertEquals(expectedPackets.size(), receivedPackets.size());
            for (int p = 0; p < testData.size(); p++) {
                byte[] expected = (byte[]) testData.get(p);
                byte[] received = (byte[]) receivedPackets.get(p);
                
                assertEquals(expected.length, received.length);
                for (int i = 0; i < expected.length; i++) {
                    assertEquals(expected[i], received[i]);
                }
            }
        }
        
    }

    /**
     * This test case executes a series of simple (non-pipelined) POST requests 
     * over multiple connections that do not meet the target server expectations. 
     */
    public void testHttpPostsWithExpectationVerification() throws Exception {
        
        final int reqNo = 3;
        final List responses = new ArrayList(reqNo);
        
        // Initialize the server-side request handler
        this.server.registerHandler("*", new HttpRequestHandler() {

            public void handle(
                    final HttpRequest request, 
                    final HttpResponse response, 
                    final HttpContext context) throws HttpException, IOException {
                
                StringEntity outgoing = new StringEntity("No content"); 
                response.setEntity(outgoing);
            }
            
        });
        
        this.server.setExpectationVerifier(new HttpExpectationVerifier() {

            public void verify(
                    final HttpRequest request, 
                    final HttpResponse response, 
                    final HttpContext context) throws HttpException {
                Header someheader = request.getFirstHeader("Secret");
                if (someheader != null) {
                    int secretNumber;
                    try {
                        secretNumber = Integer.parseInt(someheader.getValue());
                    } catch (NumberFormatException ex) {
                        response.setStatusCode(HttpStatus.SC_BAD_REQUEST);
                        return;
                    }
                    if (secretNumber < 2) {
                        response.setStatusCode(HttpStatus.SC_EXPECTATION_FAILED);
                        ByteArrayEntity outgoing = new ByteArrayEntity(
                                EncodingUtils.getAsciiBytes("Wrong secret number")); 
                        response.setEntity(outgoing);
                    }
                }
            }
            
        });

        // Activate 'expect: continue' handshake
        this.client.getParams().setBooleanParameter(HttpProtocolParams.USE_EXPECT_CONTINUE, true);
        // Initialize the client side request executor
        this.client.setHttpRequestExecutionHandler(new HttpRequestExecutionHandler() {

            public void initalizeContext(final HttpContext context, final Object attachment) {
                context.setAttribute("LIST", (List) attachment);
                context.setAttribute("REQ-COUNT", new Integer(0));
                context.setAttribute("RES-COUNT", new Integer(0));
            }

            public HttpRequest submitRequest(final HttpContext context) {
                int i = ((Integer) context.getAttribute("REQ-COUNT")).intValue();
                HttpPost post = null;
                if (i < reqNo) {
                    post = new HttpPost("/");
                    post.addHeader("Secret", Integer.toString(i));
                    ByteArrayEntity outgoing = new ByteArrayEntity(
                            EncodingUtils.getAsciiBytes("No content")); 
                    post.setEntity(outgoing);
                    
                    context.setAttribute("REQ-COUNT", new Integer(i + 1));
                }
                return post;
            }
            
            public void handleResponse(final HttpResponse response, final HttpContext context) {
                NHttpConnection conn = (NHttpConnection) context.getAttribute(
                        HttpExecutionContext.HTTP_CONNECTION);
                
                List list = (List) context.getAttribute("LIST");
                int i = ((Integer) context.getAttribute("RES-COUNT")).intValue();
                i++;
                context.setAttribute("RES-COUNT", new Integer(i));
                
                HttpEntity entity = response.getEntity();
                if (entity != null) {
                    try {
                        entity.consumeContent();
                    } catch (IOException ex) {
                        fail(ex.getMessage());
                    }
                }
                
                list.add(response);

                if (i < reqNo) {
                    conn.requestInput();
                } else {
                    try {
                        conn.close();
                    } catch (IOException ex) {
                        fail(ex.getMessage());
                    }
                }
            }
            
        });
        
        this.server.start();
        this.client.start();
        
        InetSocketAddress serverAddress = (InetSocketAddress) this.server.getSocketAddress();
        
        this.client.openConnection(
                new InetSocketAddress("localhost", serverAddress.getPort()), 
                responses);
     
        this.client.await(1, 1000);
        
        this.client.shutdown();
        this.server.shutdown();

        assertEquals(reqNo, responses.size());
        HttpResponse response = (HttpResponse) responses.get(0);
        assertEquals(HttpStatus.SC_EXPECTATION_FAILED, response.getStatusLine().getStatusCode());
        response = (HttpResponse) responses.get(1);
        assertEquals(HttpStatus.SC_EXPECTATION_FAILED, response.getStatusLine().getStatusCode());
        response = (HttpResponse) responses.get(2);
        assertEquals(HttpStatus.SC_OK, response.getStatusLine().getStatusCode());
    }
    
}
