/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/oac.hc3x/tags/HTTPCLIENT_3_0_RC3/src/test/org/apache/commons/httpclient/TestEntityEnclosingMethod.java $
 * $Revision: 161963 $
 * $Date: 2005-04-19 22:25:06 +0200 (Tue, 19 Apr 2005) $
 *
 * ====================================================================
 *
 *  Copyright 2003-2004 The Apache Software Foundation
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.commons.httpclient;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.RequestEntity;
import org.apache.commons.httpclient.server.AuthRequestHandler;
import org.apache.commons.httpclient.server.HttpRequestHandlerChain;
import org.apache.commons.httpclient.server.HttpServiceHandler;

/**
 * Tests specific to entity enclosing methods.
 *
 * @author Oleg Kalnichevski
 * @version $Id: TestEntityEnclosingMethod.java 161963 2005-04-19 20:25:06Z olegk $
 */
public class TestEntityEnclosingMethod extends HttpClientTestBase {

    public TestEntityEnclosingMethod(String testName) throws IOException {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestEntityEnclosingMethod.class);
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestEntityEnclosingMethod.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------------------ Tests
    
    public void testEnclosedEntityAutoLength() throws Exception {
        String inputstr = "This is a test message";
        byte[] input = inputstr.getBytes("US-ASCII");
        InputStream instream = new ByteArrayInputStream(input);
        
        RequestEntity requestentity = new InputStreamRequestEntity(
                instream, InputStreamRequestEntity.CONTENT_LENGTH_AUTO); 
        PostMethod method = new PostMethod("/");
        method.setRequestEntity(requestentity);
        this.server.setHttpService(new EchoService());
        try {
            this.client.executeMethod(method);
            assertEquals(200, method.getStatusCode());
            String body = method.getResponseBodyAsString();
            assertEquals(inputstr, body);
            assertNull(method.getRequestHeader("Transfer-Encoding"));
            assertNotNull(method.getRequestHeader("Content-Length"));
            assertEquals(input.length, Integer.parseInt(
                    method.getRequestHeader("Content-Length").getValue()));
        } finally {
            method.releaseConnection();
        }
    }

    public void testEnclosedEntityExplicitLength() throws Exception {
        String inputstr = "This is a test message";
        byte[] input = inputstr.getBytes("US-ASCII");
        InputStream instream = new ByteArrayInputStream(input);
        
        RequestEntity requestentity = new InputStreamRequestEntity(
                instream, 14); 
        PostMethod method = new PostMethod("/");
        method.setRequestEntity(requestentity);
        this.server.setHttpService(new EchoService());
        try {
            this.client.executeMethod(method);
            assertEquals(200, method.getStatusCode());
            String body = method.getResponseBodyAsString();
            assertEquals("This is a test", body);
            assertNull(method.getRequestHeader("Transfer-Encoding"));
            assertNotNull(method.getRequestHeader("Content-Length"));
            assertEquals(14, Integer.parseInt(
                    method.getRequestHeader("Content-Length").getValue()));
        } finally {
            method.releaseConnection();
        }
    }

    public void testEnclosedEntityChunked() throws Exception {
        String inputstr = "This is a test message";
        byte[] input = inputstr.getBytes("US-ASCII");
        InputStream instream = new ByteArrayInputStream(input);
        
        RequestEntity requestentity = new InputStreamRequestEntity(
                instream, InputStreamRequestEntity.CONTENT_LENGTH_AUTO); 
        PostMethod method = new PostMethod("/");
        method.setRequestEntity(requestentity);
        method.setContentChunked(true);
        this.server.setHttpService(new EchoService());
        try {
            this.client.executeMethod(method);
            assertEquals(200, method.getStatusCode());
            String body = method.getResponseBodyAsString();
            assertEquals(inputstr, body);
            assertNotNull(method.getRequestHeader("Transfer-Encoding"));
            assertNull(method.getRequestHeader("Content-Length"));
        } finally {
            method.releaseConnection();
        }
    }
    
    public void testEnclosedEntityChunkedHTTP1_0() throws Exception {
        String inputstr = "This is a test message";
        byte[] input = inputstr.getBytes("US-ASCII");
        InputStream instream = new ByteArrayInputStream(input);
        
        RequestEntity requestentity = new InputStreamRequestEntity(
                instream, InputStreamRequestEntity.CONTENT_LENGTH_AUTO); 
        PostMethod method = new PostMethod("/");
        method.setRequestEntity(requestentity);
        method.setContentChunked(true);
        method.getParams().setVersion(HttpVersion.HTTP_1_0);
        this.server.setHttpService(new EchoService());
        try {
            this.client.executeMethod(method);
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException ex) {
            // expected
        } finally {
            method.releaseConnection();
        }
    }

    public void testEnclosedEntityRepeatable() throws Exception {
        String inputstr = "This is a test message";
        byte[] input = inputstr.getBytes("US-ASCII");
        InputStream instream = new ByteArrayInputStream(input);
        
        RequestEntity requestentity = new InputStreamRequestEntity(
                instream, InputStreamRequestEntity.CONTENT_LENGTH_AUTO); 
        PostMethod method = new PostMethod("/");
        method.setRequestEntity(requestentity);

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds));
        handlerchain.appendHandler(new HttpServiceHandler(new EchoService()));
        this.server.setRequestHandler(handlerchain);
        this.client.getState().setCredentials(AuthScope.ANY, creds);
        try {
            this.client.executeMethod(method);
            assertEquals(200, method.getStatusCode());
            String body = method.getResponseBodyAsString();
            assertEquals(inputstr, body);
            assertNull(method.getRequestHeader("Transfer-Encoding"));
            assertNotNull(method.getRequestHeader("Content-Length"));
            assertEquals(input.length, Integer.parseInt(
                    method.getRequestHeader("Content-Length").getValue()));
        } finally {
            method.releaseConnection();
        }
    }

    public void testEnclosedEntityNonRepeatable() throws Exception {
        String inputstr = "This is a test message";
        byte[] input = inputstr.getBytes("US-ASCII");
        InputStream instream = new ByteArrayInputStream(input);
        
        RequestEntity requestentity = new InputStreamRequestEntity(
                instream, InputStreamRequestEntity.CONTENT_LENGTH_AUTO); 
        PostMethod method = new PostMethod("/");
        method.setRequestEntity(requestentity);
        method.setContentChunked(true);

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds));
        handlerchain.appendHandler(new HttpServiceHandler(new EchoService()));
        this.server.setRequestHandler(handlerchain);
        this.client.getState().setCredentials(AuthScope.ANY, creds);
        try {
            this.client.executeMethod(method);
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException ex) {
            // expected
        } finally {
            method.releaseConnection();
        }
    }
    
    public void testEnclosedEntityNegativeLength() throws Exception {
        
        String inputstr = "This is a test message";
        byte[] input = inputstr.getBytes("US-ASCII");
        InputStream instream = new ByteArrayInputStream(input);
        
        RequestEntity requestentity = new InputStreamRequestEntity(
                instream, -14); 
        PostMethod method = new PostMethod("/");
        method.setRequestEntity(requestentity);
        method.setContentChunked(false);
        this.server.setHttpService(new EchoService());
        try {
            this.client.executeMethod(method);
            assertEquals(200, method.getStatusCode());
            String body = method.getResponseBodyAsString();
            assertEquals(inputstr, body);
            assertNotNull(method.getRequestHeader("Transfer-Encoding"));
            assertNull(method.getRequestHeader("Content-Length"));
        } finally {
            method.releaseConnection();
        }
    }

    public void testEnclosedEntityNegativeLengthHTTP1_0() throws Exception {
        
        String inputstr = "This is a test message";
        byte[] input = inputstr.getBytes("US-ASCII");
        InputStream instream = new ByteArrayInputStream(input);
        
        RequestEntity requestentity = new InputStreamRequestEntity(
                instream, -14); 
        PostMethod method = new PostMethod("/");
        method.setRequestEntity(requestentity);
        method.setContentChunked(false);
        method.getParams().setVersion(HttpVersion.HTTP_1_0);
        this.server.setHttpService(new EchoService());
        try {
            this.client.executeMethod(method);
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException ex) {
            // expected
        } finally {
            method.releaseConnection();
        }
    }
}

