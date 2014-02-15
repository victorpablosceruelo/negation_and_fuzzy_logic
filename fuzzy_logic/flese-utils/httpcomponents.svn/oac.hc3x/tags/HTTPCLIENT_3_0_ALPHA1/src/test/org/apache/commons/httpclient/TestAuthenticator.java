/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestAuthenticator.java,v 1.42 2004/04/29 14:28:29 olegk Exp $
 * $Revision: 1.42 $
 * $Date: 2004-04-29 16:28:29 +0200 (Thu, 29 Apr 2004) $
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

package org.apache.commons.httpclient;

import java.util.Map;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.auth.AuthChallengeParser;
import org.apache.commons.httpclient.auth.AuthScheme;
import org.apache.commons.httpclient.auth.AuthenticationException;
import org.apache.commons.httpclient.auth.CredentialsNotAvailableException;
import org.apache.commons.httpclient.auth.DigestScheme;
import org.apache.commons.httpclient.auth.MalformedChallengeException;
import org.apache.commons.httpclient.auth.NTLMScheme;

/**
 * Unit tests for {@link Authenticator}.
 *
 * @author Rodney Waldhoff
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @version $Id: TestAuthenticator.java 134582 2004-04-29 14:28:29Z olegk $
 */
public class TestAuthenticator extends TestNoHostBase {

    // ------------------------------------------------------------ Constructor
    public TestAuthenticator(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestAuthenticator.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestAuthenticator.class);
    }


    // ---------------------------------- Helper functions

    protected static boolean doAuthenticate(
        AuthScheme authscheme, 
        HttpMethod method, 
        HttpConnection conn,
        HttpState state, 
        boolean proxy)
       throws AuthenticationException {

        if (authscheme == null) {
            throw new IllegalArgumentException("Authentication scheme may not be null");
        }
        if (method == null) {
            throw new IllegalArgumentException("HTTP method may not be null");
        }
        if (state == null) {
            throw new IllegalArgumentException("HTTP state may not be null");
        }
        String host = null;
        if (conn != null) {
            if (proxy) {
                host = conn.getProxyHost();
            } else {
                host = conn.getVirtualHost();
                if (host == null) {
                    host = conn.getHost();
                }
            }
        }
        String realm = authscheme.getRealm();
        Credentials credentials = proxy 
            ? state.getProxyCredentials(realm, host) 
            : state.getCredentials(realm, host);
        if (credentials == null) {
            throw new CredentialsNotAvailableException("No credentials available");
        }
        String auth = authscheme.authenticate(credentials, method);
        if (auth != null) {
            String s = proxy ? "Proxy-Authorization" : "Authorization";
            Header header = new Header(s, auth, true);
            method.addRequestHeader(header);
            return true;
        } else {
            return false;
        }
    }

    private static boolean authenticate(
        AuthScheme authscheme, 
        HttpMethod method, 
        HttpConnection conn,
        HttpState state) 
        throws AuthenticationException {
        return doAuthenticate(authscheme, method, conn, state, false);
    }

    public static boolean authenticateProxy(
        AuthScheme authscheme, 
        HttpMethod method, 
        HttpConnection conn,
        HttpState state
    ) throws AuthenticationException {
       return doAuthenticate(authscheme, method, conn, state, true);
    }


    public void testCredentialConstructors() {
        try {
            new UsernamePasswordCredentials(null, null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) {
            // expected
        }
        try {
            new NTCredentials("user", "password", null, null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) {
            // expected
        }
        try {
            new NTCredentials("user", "password", "host", null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) {
            // expected
        }
        NTCredentials creds = new NTCredentials("user", null, "host", "domain");
        assertNotNull(creds.getUserName());
        assertNull(creds.getPassword());
        assertNotNull(creds.getDomain());
        assertNotNull(creds.getHost());
    }

    // ---------------------------------- Test Methods for BasicScheme Authentication

    // Moved to a separate test case based on a new testing framework

    // --------------------------------- Test Methods for DigestScheme Authentication

    public void testDigestAuthenticationWithNoCreds() throws Exception {
        String challenge = "Digest realm=\"realm1\", nonce=\"ABC123\"";
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        try {
            AuthScheme authscheme = new DigestScheme();
            authscheme.processChallenge(challenge);
            authenticate(authscheme, method, null, state);
            fail("Should have thrown CredentialsNotAvailableException");
        } catch(CredentialsNotAvailableException e) {
            // expected
        }
    }

    public void testDigestAuthenticationWithNoRealm() throws Exception {
        String challenge = "Digest";
        try {
            AuthScheme authscheme = new DigestScheme();
            authscheme.processChallenge(challenge);
            fail("Should have thrown MalformedChallengeException");
        } catch(MalformedChallengeException e) {
            // expected
        }
    }

    public void testDigestAuthenticationWithNoRealm2() throws Exception {
        String challenge = "Digest ";
        try {
            AuthScheme authscheme = new DigestScheme();
            authscheme.processChallenge(challenge);
            fail("Should have thrown MalformedChallengeException");
        } catch(MalformedChallengeException e) {
            // expected
        }
    }

    public void testDigestAuthenticationWithNullHttpState() throws Exception {
        String challenge = "Digest realm=\"realm1\", nonce=\"f2a3f18799759d4f1a1c068b92b573cb\"";
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        try {
            AuthScheme authscheme = new DigestScheme();
            authscheme.processChallenge(challenge);
            authenticate(authscheme, method, null, null);
            fail("Should have thrown IllegalArgumentException");
        } catch(IllegalArgumentException e) {
            // expected
        }
    }

    public void testDigestAuthenticationCaseInsensitivity() throws Exception {
        String challenge = "dIgEsT ReAlM=\"realm1\", nOnCE=\"f2a3F18799759D4f1a1C068b92b573cB\"";
        HttpState state = new HttpState();
        UsernamePasswordCredentials cred = new UsernamePasswordCredentials("username","password");
        state.setCredentials(null, null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WwW-AuThEnTiCaTe", challenge));
        AuthScheme authscheme = new DigestScheme();
        authscheme.processChallenge(challenge);
        assertTrue(authenticate(authscheme, method, null, state));
        assertTrue(null != method.getRequestHeader("Authorization"));
    }


    public void testDigestAuthenticationWithDefaultCreds() throws Exception {
        String challenge = "Digest realm=\"realm1\", nonce=\"f2a3f18799759d4f1a1c068b92b573cb\"";
        HttpState state = new HttpState();
        UsernamePasswordCredentials cred = new UsernamePasswordCredentials("username","password");
        state.setCredentials(null, null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        AuthScheme authscheme = new DigestScheme();
        authscheme.processChallenge(challenge);
        assertTrue(authenticate(authscheme, method, null, state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        Map table = AuthChallengeParser.extractParams(method.getRequestHeader("Authorization").getValue());
        assertEquals("username", table.get("username"));
        assertEquals("realm1", table.get("realm"));
        assertEquals("/", table.get("uri"));
        assertEquals("f2a3f18799759d4f1a1c068b92b573cb", table.get("nonce"));
        assertEquals("e95a7ddf37c2eab009568b1ed134f89a", table.get("response"));
    }

    public void testDigestAuthentication() throws Exception {
        String challenge = "Digest realm=\"realm1\", nonce=\"f2a3f18799759d4f1a1c068b92b573cb\"";
        HttpState state = new HttpState();
        UsernamePasswordCredentials cred = new UsernamePasswordCredentials("username","password");
        state.setCredentials(null, null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        AuthScheme authscheme = new DigestScheme();
        authscheme.processChallenge(challenge);
        assertTrue(authenticate(authscheme, method, null, state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        Map table = AuthChallengeParser.extractParams(method.getRequestHeader("Authorization").getValue());
        assertEquals("username", table.get("username"));
        assertEquals("realm1", table.get("realm"));
        assertEquals("/", table.get("uri"));
        assertEquals("f2a3f18799759d4f1a1c068b92b573cb", table.get("nonce"));
        assertEquals("e95a7ddf37c2eab009568b1ed134f89a", table.get("response"));
    }

    public void testDigestAuthenticationWithStaleNonce() throws Exception {
        
        String headers =
            "HTTP/1.1 401 OK\r\n" +
            "Connection: close\r\n" +
            "Content-Length: 0\r\n" +
            "WWW-Authenticate: Digest realm=\"realm1\", nonce=\"ABC123\"\r\n";
        String headers2 =
            "HTTP/1.1 401 OK\r\n" +
            "Connection: close\r\n" +
            "Content-Length: 0\r\n" +
            "WWW-Authenticate: Digest realm=\"realm1\", nonce=\"321CBA\", stale=\"true\"\r\n";
        String headers3 = 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n\r\n" +
            "stuff\r\n";
        
        SimpleHttpConnection conn = new SimpleHttpConnection();
        
        conn.addResponse(headers);
        conn.addResponse(headers2);
        conn.addResponse(headers3);
        UsernamePasswordCredentials cred = new UsernamePasswordCredentials("username","password");
        client.getState().setCredentials(null, null, cred);

        connectionManager.setConnection(conn);

        SimpleHttpMethod method = new SimpleHttpMethod();
        method.setDoAuthentication(true);
        assertEquals("Authentication failed", 200, client.executeMethod(method));
        Map table = AuthChallengeParser.extractParams(method.getRequestHeader("Authorization").getValue());
        assertEquals("username", table.get("username"));
        assertEquals("realm1", table.get("realm"));
        assertEquals("/", table.get("uri"));
        assertEquals("321CBA", table.get("nonce"));
        assertEquals("7f5948eefa115296e9279225041527b3", table.get("response"));
    }

    public void testDigestAuthenticationWithMultipleRealms() throws Exception {
        String challenge1 = "Digest realm=\"realm1\", nonce=\"abcde\"";
        String challenge2 = "Digest realm=\"realm2\", nonce=\"123546\"";
        HttpState state = new HttpState();
        UsernamePasswordCredentials cred = new UsernamePasswordCredentials("username","password");
        state.setCredentials("realm1", null, cred);
        UsernamePasswordCredentials cred2 = new UsernamePasswordCredentials("uname2","password2");
        state.setCredentials("realm2", null, cred2);
        AuthScheme authscheme1 = new DigestScheme();
        authscheme1.processChallenge(challenge1);
        AuthScheme authscheme2 = new DigestScheme();
        authscheme2.processChallenge(challenge2);
        {
            HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate",challenge1));
            assertTrue(authenticate(authscheme1, method, null, state));
            assertTrue(null != method.getRequestHeader("Authorization"));
            Map table = AuthChallengeParser.extractParams(method.getRequestHeader("Authorization").getValue());
            assertEquals("username", table.get("username"));
            assertEquals("realm1", table.get("realm"));
            assertEquals("/", table.get("uri"));
            assertEquals("abcde", table.get("nonce"));
            assertEquals("786f500303eac1478f3c2865e676ed68", table.get("response"));
        }
        {
            HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate",challenge2));
            assertTrue(authenticate(authscheme2, method, null, state));
            assertTrue(null != method.getRequestHeader("Authorization"));
            Map table = AuthChallengeParser.extractParams(method.getRequestHeader("Authorization").getValue());
            assertEquals("uname2", table.get("username"));
            assertEquals("realm2", table.get("realm"));
            assertEquals("/", table.get("uri"));
            assertEquals("123546", table.get("nonce"));
            assertEquals("0283edd9ef06a38b378b3b74661391e9", table.get("response"));
        }
    }

    /** 
     * Test digest authentication using the MD5-sess algorithm.
     */
    public void testDigestAuthenticationMD5Sess() throws Exception {
        // Example using Digest auth with MD5-sess

        String realm="realm";
        String username="username";
        String password="password";
        String nonce="e273f1776275974f1a120d8b92c5b3cb";

        String challenge="Digest realm=\"" + realm + "\", "
            + "nonce=\"" + nonce + "\", "
            + "opaque=\"SomeString\", "
            + "stale=false, "
            + "algorithm=MD5-sess, "
            + "qop=\"auth,auth-int\""; // we pass both but expect auth to be used

        HttpState state = new HttpState();
        UsernamePasswordCredentials cred =
            new UsernamePasswordCredentials(username, password);
        state.setCredentials(realm, null, cred);
        AuthScheme authscheme = new DigestScheme();
        authscheme.processChallenge(challenge);
        HttpMethod method =
            new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        assertTrue(authenticate(
            authscheme, method, null, state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        Map table = AuthChallengeParser.extractParams(method.getRequestHeader("Authorization").getValue());
        assertEquals(username, table.get("username"));
        assertEquals(realm, table.get("realm"));
        assertEquals("MD5-sess", table.get("algorithm"));
        assertEquals("/", table.get("uri"));
        assertEquals(nonce, table.get("nonce"));
        assertEquals(1, Integer.parseInt((String) table.get("nc"),16));
        assertTrue(null != table.get("cnonce"));
        assertEquals("SomeString", table.get("opaque"));
        assertEquals("auth", table.get("qop"));
        //@TODO: add better check
        assertTrue(null != table.get("response")); 
    }
    
    /** 
     * Test digest authentication using the MD5-sess algorithm.
     */
    public void testDigestAuthenticationMD5SessNoQop() throws Exception {
        // Example using Digest auth with MD5-sess

        String realm="realm";
        String username="username";
        String password="password";
        String nonce="e273f1776275974f1a120d8b92c5b3cb";

        String challenge="Digest realm=\"" + realm + "\", "
            + "nonce=\"" + nonce + "\", "
            + "opaque=\"SomeString\", "
            + "stale=false, "
            + "algorithm=MD5-sess";

        HttpState state = new HttpState();
        UsernamePasswordCredentials cred =
            new UsernamePasswordCredentials(username, password);
        state.setCredentials(realm, null, cred);
        AuthScheme authscheme = new DigestScheme();
        authscheme.processChallenge(challenge);
        HttpMethod method =
            new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        assertTrue(authenticate(
            authscheme, method, null, state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        Map table = AuthChallengeParser.extractParams(method.getRequestHeader("Authorization").getValue());
        assertEquals(username, table.get("username"));
        assertEquals(realm, table.get("realm"));
        assertEquals("MD5-sess", table.get("algorithm"));
        assertEquals("/", table.get("uri"));
        assertEquals(nonce, table.get("nonce"));
        assertTrue(null == table.get("nc"));
        assertEquals("SomeString", table.get("opaque"));
        assertTrue(null == table.get("qop"));
        //@TODO: add better check
        assertTrue(null != table.get("response")); 
    }
    
    /** 
     * Test digest authentication with invalud qop value
     */
    public void testDigestAuthenticationMD5SessInvalidQop() throws Exception {
        // Example using Digest auth with MD5-sess

        String realm="realm";
        String username="username";
        String password="password";
        String nonce="e273f1776275974f1a120d8b92c5b3cb";

        String challenge="Digest realm=\"" + realm + "\", "
            + "nonce=\"" + nonce + "\", "
            + "opaque=\"SomeString\", "
            + "stale=false, "
            + "algorithm=MD5-sess, "
            + "qop=\"jakarta\""; // jakarta is an invalid qop value

        HttpState state = new HttpState();
        UsernamePasswordCredentials cred =
            new UsernamePasswordCredentials(username, password);
        state.setCredentials(realm, null, cred);
        try {
            AuthScheme authscheme = new DigestScheme();
            authscheme.processChallenge(challenge);
            fail("MalformedChallengeException exception expected due to invalid qop value");
        } catch(MalformedChallengeException e) {
            /* expected */
        }
    }    

    

    // --------------------------------- Test Methods for NTLM Authentication

    public void testNTLMAuthenticationWithNoCreds() {
        String challenge = "NTLM";
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        method.addRequestHeader("Host", "host");
        try {
            AuthScheme authscheme = new NTLMScheme(challenge);
            authenticate(authscheme, method, null, state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testNTLMAuthenticationWithNullHttpState() throws Exception {
        String challenge = "NTLM";
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        method.addRequestHeader("Host", "host");
        try {
            AuthScheme authscheme = new NTLMScheme(challenge);
            authenticate(authscheme, method, null, null);
            fail("Should have thrown IllegalArgumentException");
        } catch(IllegalArgumentException e) {
            // expected
        }
    }

    public void testNTLMAuthenticationCaseInsensitivity() throws Exception {
        String challenge = "nTlM";
        HttpState state = new HttpState();
        NTCredentials cred = new NTCredentials("username","password", "host",
                "domain");
        state.setCredentials(null, null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WwW-AuThEnTiCaTe", challenge));
        AuthScheme authscheme = new NTLMScheme(challenge);
        assertTrue(authenticate(authscheme, method, null, state));
        assertTrue(null != method.getRequestHeader("Authorization"));
    }

    public void testNTLMAuthenticationResponse1() throws Exception {
        String challenge = "NTLM";
        String expected = "NTLM TlRMTVNTUAABAAAABlIAAAYABgAkAAAABAAEACAAAABIT" +
            "1NURE9NQUlO";
        HttpState state = new HttpState();
        NTCredentials cred = new NTCredentials("username","password", "host",
                "domain");
        state.setCredentials(null, null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        AuthScheme authscheme = new NTLMScheme(challenge);
        assertTrue(authenticate(authscheme, method, null, state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        assertEquals(expected,
                method.getRequestHeader("Authorization").getValue());
    }
    
    public void testNTLMAuthenticationResponse2() throws Exception {
        String challenge = 
            "NTLM TlRMTVNTUAACAAAACgAKADAAAAAGgoEAPc4kP4LtCV8AAAAAAAAAAJ4AngA" +
            "6AAAASU5UUkFFUEhPWAIAFABJAE4AVABSAEEARQBQAEgATwBYAAEAEgBCAE8AQQB" +
            "SAEQAUgBPAE8ATQAEACgAaQBuAHQAcgBhAGUAcABoAG8AeAAuAGUAcABoAG8AeAA" +
            "uAGMAbwBtAAMAPABCAG8AYQByAGQAcgBvAG8AbQAuAGkAbgB0AHIAYQBlAHAAaAB" +
            "vAHgALgBlAHAAaABvAHgALgBjAG8AbQAAAAAA";

        String expected = "NTLM TlRMTVNTUAADAAAAGAAYAFIAAAAAAAAAagAAAAYABgB" +
            "AAAAACAAIAEYAAAAEAAQATgAAAAAAAABqAAAABlIAAERPTUFJTlVTRVJOQU1FSE" +
            "9TVAaC+vLxUEHnUtpItj9Dp4kzwQfd61Lztg==";
        HttpState state = new HttpState();
        NTCredentials cred = new NTCredentials("username","password", "host",
                "domain");
        state.setCredentials(null, null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        AuthScheme authscheme = new NTLMScheme(challenge);
        assertTrue(authenticate(authscheme, method, null, state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        assertEquals(expected,
                method.getRequestHeader("Authorization").getValue());
    }
    
    public void testNTLMAuthenticationWithDefaultCreds() throws Exception {
        String challenge = 
            "NTLM TlRMTVNTUAACAAAACgAKADAAAAAGgoEAPc4kP4LtCV8AAAAAAAAAAJ4AngA" +
            "6AAAASU5UUkFFUEhPWAIAFABJAE4AVABSAEEARQBQAEgATwBYAAEAEgBCAE8AQQB" +
            "SAEQAUgBPAE8ATQAEACgAaQBuAHQAcgBhAGUAcABoAG8AeAAuAGUAcABoAG8AeAA" +
            "uAGMAbwBtAAMAPABCAG8AYQByAGQAcgBvAG8AbQAuAGkAbgB0AHIAYQBlAHAAaAB" +
            "vAHgALgBlAHAAaABvAHgALgBjAG8AbQAAAAAA";
        String expected = "NTLM TlRMTVNTUAADAAAAGAAYAFIAAAAAAAAAagAAAAYABgB" +
            "AAAAACAAIAEYAAAAEAAQATgAAAAAAAABqAAAABlIAAERPTUFJTlVTRVJOQU1FSE" +
            "9TVAaC+vLxUEHnUtpItj9Dp4kzwQfd61Lztg==";
        HttpState state = new HttpState();
        NTCredentials cred = new NTCredentials("username","password", "host",
                "domain");
        state.setCredentials(null, null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate", challenge));
        method.addRequestHeader("Host", "host");
        AuthScheme authscheme = new NTLMScheme(challenge);
        assertTrue(authenticate(authscheme, method, null, state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        assertEquals(expected,
                method.getRequestHeader("Authorization").getValue());
    }
    
    public void testNTLMAuthenticationRetry() throws Exception {
        NTCredentials cred = new NTCredentials("username", "password", "host", "domain");
        client.getState().setCredentials(null, null, cred);
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" +
            "WWW-Authenticate: NTLM\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n");
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" +
            "WWW-Authenticate: NTLM TlRMTVNTUAACAAAAAAAAACgAAAABggAAU3J2Tm9uY2UAAAAAAAAAAA==\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n");
        conn.addResponse(
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n\r\n" +
            "stuff\r\n");
        client.executeMethod(method);
        assertNull(method.getResponseHeader("WWW-Authenticate"));
        assertEquals(200, method.getStatusCode());
    }

    /** 
     * Test that the Unauthorized response is returned when doAuthentication is false.
     */
    public void testDoAuthenticateFalse() throws Exception {
        client.getState().setCredentials(null, "Protected",
                new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        method.setDoAuthentication(false);
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" + 
            "WWW-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n");
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"); 
        client.executeMethod(method);
        assertNotNull(method.getResponseHeader("WWW-Authenticate"));
        assertNull(method.getRequestHeader("Authorization"));
        assertEquals(401, method.getStatusCode());

    }


    /** 
     */
    public void testInvalidCredentials() throws Exception {
        client.getState().setCredentials(null, "Protected", new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        method.setDoAuthentication(false);
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" + 
            "WWW-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        client.executeMethod(method);
        assertEquals(401, method.getStatusCode());
    }


    // --------------------------------- Test Methods for Multiple Authentication

    public void testMultipleChallengeBasic() throws Exception {
        client.getState().setCredentials(null, null, new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" + 
            "WWW-Authenticate: Unsupported\r\n" +
            "WWW-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        client.executeMethod(method);
        Header authHeader = method.getRequestHeader("Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Basic"));
    }

    public void testMultipleChallengeBasicLongRealm() throws Exception {
        client.getState().setCredentials(null, null, new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" + 
            "WWW-Authenticate: Unsupported\r\n" +
            "WWW-Authenticate: Basic realm=\"This site is protected.  We put this message into the realm string, against all reasonable rationale, so that users would see it in the authentication dialog generated by your browser.\"\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                ); 
        client.executeMethod(method);
        Header authHeader = method.getRequestHeader("Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Basic"));
    }




    public void testMultipleChallengeDigest() throws Exception {
        client.getState().setCredentials(null, null, new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" + 
            "WWW-Authenticate: Unsupported\r\n" +
            "WWW-Authenticate: Digest realm=\"Protected\", nonce=\"f2a3f18799759d4f1a1c068b92b573cb\"\r\n" +
            "WWW-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                ); 
        client.executeMethod(method);
        Header authHeader = method.getRequestHeader("Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Digest"));
    }


    public void testMultipleProxyChallengeBasic() throws Exception {
        client.getState().setProxyCredentials("Protected", null, new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(
            "HTTP/1.1 407 Proxy Authentication Required\r\n" + 
            "Proxy-Authenticate: Basic realm=\"Protected\", nonce=\"f2a3f18799759d4f1a1c068b92b573cb\"\r\n" +
            "Proxy-Authenticate: Unsupported\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                ); 
        client.executeMethod(method);
        Header authHeader = method.getRequestHeader("Proxy-Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Basic"));
    }


    public void testMultipleProxyChallengeDigest() throws Exception {
        client.getState().setProxyCredentials("Protected", null, new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(
            "HTTP/1.1 407 Proxy Authentication Required\r\n" + 
            "Proxy-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Proxy-Authenticate: Digest realm=\"Protected\", nonce=\"f2a3f18799759d4f1a1c068b92b573cb\"\r\n" +
            "Proxy-Authenticate: Unsupported\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                ); 
        client.executeMethod(method);
        Header authHeader = method.getRequestHeader("Proxy-Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Digest"));
    }


    // --------------------------------- Test Methods for Selecting Credentials
    
    public void testDefaultCredentials() throws Exception {
        HttpState state = new HttpState();
        Credentials expected = new UsernamePasswordCredentials("name", "pass");
        state.setCredentials(null, null, expected);
        Credentials got = state.getCredentials("realm", "host");
        assertEquals(got, expected);
    }
    
    public void testRealmCredentials() throws Exception {
        HttpState state = new HttpState();
        Credentials expected = new UsernamePasswordCredentials("name", "pass");
        state.setCredentials("realm", "host", expected);
        Credentials got = state.getCredentials("realm", "host");
        assertEquals(expected, got);
    }
    
    public void testHostCredentials() throws Exception {
        HttpState state = new HttpState();
        Credentials expected = new UsernamePasswordCredentials("name", "pass");
        state.setCredentials(null, "host", expected);
        Credentials got = state.getCredentials("realm", "host");
        assertEquals(expected, got);
    }
    
    public void testBothCredentials() throws Exception {
        HttpState state = new HttpState();
        Credentials expected = new UsernamePasswordCredentials("name", "pass");
        state.setCredentials("realm", "host", expected);
        Credentials got = state.getCredentials("realm", "host");
        assertEquals(expected, got);
    }
    
    public void testWrongHostCredentials() throws Exception {
        HttpState state = new HttpState();
        Credentials expected = new UsernamePasswordCredentials("name", "pass");
        state.setCredentials(null, "host1", expected);
        Credentials got = state.getCredentials("realm", "host2");
        assertNotSame(expected, got);
    }
    
    public void testWrongRealmCredentials() throws Exception {
        HttpState state = new HttpState();
        Credentials cred = new UsernamePasswordCredentials("name", "pass");
        state.setCredentials("realm1", "host", cred);
        Credentials got = state.getCredentials("realm2", "host");
        assertNotSame(cred, got);
    }
    
    public void testRealmSpoof() throws Exception {
        HttpState state = new HttpState();
        Credentials cred = new UsernamePasswordCredentials("name", "pass");
        state.setCredentials(null, "admin.apache.org", cred);
        Credentials got = state.getCredentials("admin.apache.org", "myhost");
        assertNotSame(cred, got);
    }
    
    public void testRealmSpoof2() throws Exception {
        HttpState state = new HttpState();
        Credentials cred = new UsernamePasswordCredentials("name", "pass");
        state.setCredentials(null, "whatever", cred);
        Credentials got = state.getCredentials("nullwhatever", null);
        assertNotSame(cred, got);
    }
}
