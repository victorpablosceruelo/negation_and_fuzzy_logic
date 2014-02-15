/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/auth/TestBasicAuth.java,v 1.3 2004/06/12 22:47:23 olegk Exp $
 * $Revision: 1.3 $
 * $Date: 2004-06-13 00:47:23 +0200 (Sun, 13 Jun 2004) $
 * ====================================================================
 *
 *  Copyright 1999-2004 The Apache Software Foundation
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient.auth;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpClientTestBase;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.server.HttpService;
import org.apache.commons.httpclient.server.RequestLine;
import org.apache.commons.httpclient.server.SimpleRequest;
import org.apache.commons.httpclient.server.SimpleResponse;
import org.apache.commons.httpclient.util.EncodingUtil;

/**
 * Basic authentication test cases.
 *
 * @author Oleg Kalnichevski
 * 
 * @version $Id: TestBasicAuth.java 134632 2004-06-12 22:47:23Z olegk $
 */
public class TestBasicAuth extends HttpClientTestBase {

    // ------------------------------------------------------------ Constructor
    public TestBasicAuth(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestBasicAuth.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestBasicAuth.class);
    }

    private class BasicAuthService implements HttpService {

        public BasicAuthService() {
            super();
        }

        public boolean process(final SimpleRequest request, final SimpleResponse response)
            throws IOException
        {
            Header challenge = new Header("WWW-Authenticate", "Basic realm=\"test\"");
            RequestLine requestLine = request.getRequestLine();
            Header auth = request.getFirstHeader("Authorization");
            if (auth == null) { 
                response.setStatusLine("HTTP/1.1 401 Unauthorized");
                response.addHeader(challenge);
                response.setBodyString("Authorization required");
                return true;
            }
            boolean pass = false;
            String s = auth.getValue();
            int i = s.indexOf(" ");
            if (i != -1) {
                String authtype = s.substring(0, i);
                if ("BASIC".equalsIgnoreCase(authtype)) {
                    String creds = s.substring(i + 1, s.length());
                    creds = EncodingUtil.getAsciiString(
                        Base64.decodeBase64(
                            EncodingUtil.getAsciiBytes(creds)));
                    if (creds.equals("test:test")) {
                        pass = true;    
                    }
                }
            }
            if (!pass) {
                response.setStatusLine("HTTP/1.1 403 Forbidden");
                response.addHeader(challenge);
                response.setBodyString("Access forbidden");
                return true;
            }
            response.setStatusLine("HTTP/1.1 200 OK");
            response.setBodyString("Authorization successful");
            return true;
        }
    }

    private class BasicAuthService2 implements HttpService {

        public BasicAuthService2() {
            super();
        }

        public boolean process(final SimpleRequest request, final SimpleResponse response)
            throws IOException
        {
            Header challenge = new Header("WWW-Authenticate", "Basic realm=\"test2\"");
            RequestLine requestLine = request.getRequestLine();
            Header auth = request.getFirstHeader("Authorization");
            if (auth == null) { 
                response.setStatusLine("HTTP/1.1 401 Unauthorized");
                response.addHeader(challenge);
                response.setBodyString("Authorization required");
                return true;
            }
            boolean pass = false;
            String s = auth.getValue();
            int i = s.indexOf(" ");
            if (i != -1) {
                String authtype = s.substring(0, i);
                if ("BASIC".equalsIgnoreCase(authtype)) {
                    String creds = s.substring(i + 1, s.length());
                    creds = EncodingUtil.getAsciiString(
                        Base64.decodeBase64(
                            EncodingUtil.getAsciiBytes(creds)));
                    if (creds.equals("test2:test2")) {
                        pass = true;    
                    }
                }
            }
            if (!pass) {
                response.setStatusLine("HTTP/1.1 403 Forbidden");
                response.addHeader(challenge);
                response.setBodyString("Access forbidden");
                return true;
            }
            response.setStatusLine("HTTP/1.1 200 OK");
            response.setBodyString("Authorization successful");
            return true;
        }
    }

    private class BasicAuthService3 implements HttpService {

        public BasicAuthService3() {
            super();
        }

        public boolean process(final SimpleRequest request, final SimpleResponse response)
            throws IOException
        {
            Header challenge = new Header("WwW-AuThEnTiCaTe", "bAsIc ReAlM=\"test\"");
            RequestLine requestLine = request.getRequestLine();
            Header auth = request.getFirstHeader("Authorization");
            if (auth == null) { 
                response.setStatusLine("HTTP/1.1 401 Unauthorized");
                response.addHeader(challenge);
                response.setBodyString("Authorization required");
                return true;
            }
            boolean pass = false;
            String s = auth.getValue();
            int i = s.indexOf(" ");
            if (i != -1) {
                String authtype = s.substring(0, i);
                if ("BASIC".equalsIgnoreCase(authtype)) {
                    String creds = s.substring(i + 1, s.length());
                    creds = EncodingUtil.getAsciiString(
                        Base64.decodeBase64(
                            EncodingUtil.getAsciiBytes(creds)));
                    if (creds.equals("test:test")) {
                        pass = true;    
                    }
                }
            }
            if (!pass) {
                response.setStatusLine("HTTP/1.1 403 Forbidden");
                response.addHeader(challenge);
                response.setBodyString("Access forbidden");
                return true;
            }
            response.setStatusLine("HTTP/1.1 200 OK");
            response.setBodyString("Authorization successful");
            return true;
        }
    }

    public void testBasicAuthenticationWithNoCreds() throws IOException {
        this.server.setHttpService(new BasicAuthService());
        GetMethod httpget = new GetMethod("/test/");
        try {
            this.client.executeMethod(httpget);
            assertNotNull(httpget.getStatusLine());
            assertEquals(HttpStatus.SC_UNAUTHORIZED, httpget.getStatusLine().getStatusCode());
            AuthState authstate = httpget.getHostAuthState();
            assertNotNull(authstate.getAuthScheme());
            assertTrue(authstate.getAuthScheme() instanceof BasicScheme);
            assertEquals("test", authstate.getRealm());
        } finally {
            httpget.releaseConnection();
        }
    }

    public void testBasicAuthenticationWithNoRealm() {
        String challenge = "Basic";
        try {
            AuthScheme authscheme = new BasicScheme();
            authscheme.processChallenge(challenge);
            fail("Should have thrown MalformedChallengeException");
        } catch(MalformedChallengeException e) {
            // expected
        }
    }

    public void testBasicAuthenticationWith88591Chars() throws Exception {
        int[] germanChars = { 0xE4, 0x2D, 0xF6, 0x2D, 0xFc };
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < germanChars.length; i++) {
            buffer.append((char)germanChars[i]); 
        }
        
        UsernamePasswordCredentials credentials = new UsernamePasswordCredentials("dh", buffer.toString());
        assertEquals("Basic ZGg65C32Lfw=", 
            BasicScheme.authenticate(credentials, "ISO-8859-1"));
    }
    
    public void testBasicAuthenticationWithDefaultCreds() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials("test", "test"));
        this.client.setState(state);
        this.server.setHttpService(new BasicAuthService());
        GetMethod httpget = new GetMethod("/test/");
        try {
            this.client.executeMethod(httpget);
        } finally {
            httpget.releaseConnection();
        }
        assertNotNull(httpget.getStatusLine());
        assertEquals(HttpStatus.SC_OK, httpget.getStatusLine().getStatusCode());
        Header auth = httpget.getRequestHeader("Authorization");
        assertNotNull(auth);
        String expected = "Basic " + EncodingUtil.getAsciiString(
            Base64.encodeBase64(EncodingUtil.getAsciiBytes("test:test")));
        assertEquals(expected, auth.getValue());
        AuthState authstate = httpget.getHostAuthState();
        assertNotNull(authstate.getAuthScheme());
        assertTrue(authstate.getAuthScheme() instanceof BasicScheme);
        assertEquals("test", authstate.getRealm());
    }

    public void testBasicAuthentication() throws Exception {
        HttpState state = new HttpState();
        AuthScope authscope = new AuthScope(
            this.server.getLocalAddress(), 
            this.server.getLocalPort(),
            "test");
        state.setCredentials(authscope, new UsernamePasswordCredentials("test", "test"));
        this.client.setState(state);
        this.server.setHttpService(new BasicAuthService());
        GetMethod httpget = new GetMethod("/test/");
        try {
            this.client.executeMethod(httpget);
        } finally {
            httpget.releaseConnection();
        }
        assertNotNull(httpget.getStatusLine());
        assertEquals(HttpStatus.SC_OK, httpget.getStatusLine().getStatusCode());
        Header auth = httpget.getRequestHeader("Authorization");
        assertNotNull(auth);
        String expected = "Basic " + EncodingUtil.getAsciiString(
            Base64.encodeBase64(EncodingUtil.getAsciiBytes("test:test")));
        assertEquals(expected, auth.getValue());
        AuthState authstate = httpget.getHostAuthState();
        assertNotNull(authstate.getAuthScheme());
        assertTrue(authstate.getAuthScheme() instanceof BasicScheme);
        assertEquals("test", authstate.getRealm());
    }

    public void testBasicAuthenticationWithInvalidCredentials() throws Exception {
        HttpState state = new HttpState();
        AuthScope authscope = new AuthScope(
            this.server.getLocalAddress(), 
            this.server.getLocalPort(),
            "test");
        state.setCredentials(authscope, new UsernamePasswordCredentials("test", "stuff"));
        this.client.setState(state);
        this.server.setHttpService(new BasicAuthService());
        GetMethod httpget = new GetMethod("/test/");
        try {
            this.client.executeMethod(httpget);
        } finally {
            httpget.releaseConnection();
        }
        assertNotNull(httpget.getStatusLine());
        assertEquals(HttpStatus.SC_FORBIDDEN, httpget.getStatusLine().getStatusCode());
        AuthState authstate = httpget.getHostAuthState();
        assertNotNull(authstate.getAuthScheme());
        assertTrue(authstate.getAuthScheme() instanceof BasicScheme);
        assertEquals("test", authstate.getRealm());
    }

    public void testBasicAuthenticationWithMutlipleRealms() throws Exception {
        HttpState state = new HttpState();
        AuthScope realm1 = new AuthScope(
            this.server.getLocalAddress(), 
            this.server.getLocalPort(),
            "test");
        AuthScope realm2 = new AuthScope(
            this.server.getLocalAddress(), 
            this.server.getLocalPort(),
            "test2");
        state.setCredentials(realm1, new UsernamePasswordCredentials("test","test"));
        state.setCredentials(realm2, new UsernamePasswordCredentials("test2","test2"));
        this.client.setState(state);
        {
            this.server.setHttpService(new BasicAuthService());
            GetMethod httpget = new GetMethod("/test/");
            try {
                this.client.executeMethod(httpget);
            } finally {
                httpget.releaseConnection();
            }
            assertNotNull(httpget.getStatusLine());
            assertEquals(HttpStatus.SC_OK, httpget.getStatusLine().getStatusCode());
            Header auth = httpget.getRequestHeader("Authorization");
            assertNotNull(auth);
            String expected = "Basic " + EncodingUtil.getAsciiString(
                Base64.encodeBase64(EncodingUtil.getAsciiBytes("test:test")));
            assertEquals(expected, auth.getValue());
            AuthState authstate = httpget.getHostAuthState();
            assertNotNull(authstate.getAuthScheme());
            assertTrue(authstate.getAuthScheme() instanceof BasicScheme);
            assertEquals("test", authstate.getRealm());
        }
        {
            this.server.setHttpService(new BasicAuthService2());
            GetMethod httpget = new GetMethod("/test2/");
            try {
                this.client.executeMethod(httpget);
            } finally {
                httpget.releaseConnection();
            }
            assertNotNull(httpget.getStatusLine());
            assertEquals(HttpStatus.SC_OK, httpget.getStatusLine().getStatusCode());
            Header auth = httpget.getRequestHeader("Authorization");
            assertNotNull(auth);
            String expected = "Basic " + EncodingUtil.getAsciiString(
                Base64.encodeBase64(EncodingUtil.getAsciiBytes("test2:test2")));
            assertEquals(expected, auth.getValue());
            AuthState authstate = httpget.getHostAuthState();
            assertNotNull(authstate.getAuthScheme());
            assertTrue(authstate.getAuthScheme() instanceof BasicScheme);
            assertEquals("test2", authstate.getRealm());
        }
    }

    public void testPreemptiveAuthorizationTrueWithCreds() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials("test", "test"));
        this.client.setState(state);
        this.client.getParams().setAuthenticationPreemptive(true);
        this.server.setHttpService(new BasicAuthService());
        GetMethod httpget = new GetMethod("/test/");
        try {
            this.client.executeMethod(httpget);
        } finally {
            httpget.releaseConnection();
        }
        assertNotNull(httpget.getStatusLine());
        assertEquals(HttpStatus.SC_OK, httpget.getStatusLine().getStatusCode());
        Header auth = httpget.getRequestHeader("Authorization");
        assertNotNull(auth);
        String expected = "Basic " + EncodingUtil.getAsciiString(
            Base64.encodeBase64(EncodingUtil.getAsciiBytes("test:test")));
        assertEquals(expected, auth.getValue());
        AuthState authstate = httpget.getHostAuthState();
        assertNotNull(authstate.getAuthScheme());
        assertTrue(authstate.getAuthScheme() instanceof BasicScheme);
        assertNull(authstate.getRealm());
        assertTrue(authstate.isPreemptive());
    }

    public void testPreemptiveAuthorizationTrueWithoutCreds() throws Exception {
        HttpState state = new HttpState();
        this.client.setState(state);
        this.client.getParams().setAuthenticationPreemptive(true);
        this.server.setHttpService(new BasicAuthService());
        GetMethod httpget = new GetMethod("/test/");
        try {
            this.client.executeMethod(httpget);
        } finally {
            httpget.releaseConnection();
        }
        assertNotNull(httpget.getStatusLine());
        assertEquals(HttpStatus.SC_UNAUTHORIZED, httpget.getStatusLine().getStatusCode());
        Header auth = httpget.getRequestHeader("Authorization");
        assertNull(auth);
        AuthState authstate = httpget.getHostAuthState();
        assertNotNull(authstate.getAuthScheme());
        assertTrue(authstate.getAuthScheme() instanceof BasicScheme);
        assertNotNull(authstate.getRealm());
        assertFalse(authstate.isPreemptive());
    }

    public void testBasicAuthenticationCaseInsensitivity() throws Exception {
        HttpState state = new HttpState();
        AuthScope authscope = new AuthScope(
            this.server.getLocalAddress(), 
            this.server.getLocalPort(),
            "test");
        state.setCredentials(authscope, new UsernamePasswordCredentials("test", "test"));
        this.client.setState(state);
        this.server.setHttpService(new BasicAuthService3());
        GetMethod httpget = new GetMethod("/test/");
        try {
            this.client.executeMethod(httpget);
        } finally {
            httpget.releaseConnection();
        }
        assertNotNull(httpget.getStatusLine());
        assertEquals(HttpStatus.SC_OK, httpget.getStatusLine().getStatusCode());
        Header auth = httpget.getRequestHeader("Authorization");
        assertNotNull(auth);
        String expected = "Basic " + EncodingUtil.getAsciiString(
            Base64.encodeBase64(EncodingUtil.getAsciiBytes("test:test")));
        assertEquals(expected, auth.getValue());
    }


    public void testCustomAuthorizationHeader() throws Exception {
        String authResponse = "Basic " + EncodingUtil.getAsciiString(
            Base64.encodeBase64(EncodingUtil.getAsciiBytes("test:test")));
        this.server.setHttpService(new BasicAuthService());
        GetMethod httpget = new GetMethod("/test/");
        httpget.addRequestHeader(new Header("Authorization", authResponse));
        try {
            this.client.executeMethod(httpget);
        } finally {
            httpget.releaseConnection();
        }
        assertNotNull(httpget.getStatusLine());
        assertEquals(HttpStatus.SC_OK, httpget.getStatusLine().getStatusCode());
    }
}
