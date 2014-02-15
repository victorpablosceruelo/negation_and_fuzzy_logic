/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestAuthenticator.java,v 1.21 2003/01/23 22:48:25 jsdever Exp $
 * $Revision: 1.21 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Tomcat", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.util.Hashtable;
import java.util.StringTokenizer;
import org.apache.commons.httpclient.util.Base64;

/**
 * Unit tests for {@link Authenticator}.
 *
 * @author Rodney Waldhoff
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @version $Id: TestAuthenticator.java 134019 2003-01-23 22:48:49Z jsdever $
 */
public class TestAuthenticator extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestAuthenticator(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestAuthenticator.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- Utility Methods

    private void checkAuthorization(UsernamePasswordCredentials cred, String methodName, String auth) throws Exception {
        Hashtable table = new Hashtable();
        StringTokenizer tokenizer = new StringTokenizer(auth, ",=\"");
        while(tokenizer.hasMoreTokens()){
            String key = null;
            String value = null;
            if(tokenizer.hasMoreTokens())
                key = tokenizer.nextToken();
            if(tokenizer.hasMoreTokens())
                value = tokenizer.nextToken();
            if(key != null && value != null){
                table.put(key.trim(),value.trim());
            }
        }
        String response = (String) table.get("response");
        table.put( "methodname", methodName );
        String digest = Authenticator.createDigest(cred.getUserName(),cred.getPassword(), table);
        assertEquals(response, digest);
    }


    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestAuthenticator.class);
    }


    // ---------------------------------- Test Methods for Basic Authentication

    public void testBasicAuthenticationWithNoCreds() {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm1\""));
        try {
            Authenticator.authenticate(method,state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testBasicAuthenticationWithNoRealm() {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic"));
        try {
            Authenticator.authenticate(method,state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testBasicAuthenticationWithNoRealm2() {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic "));
        try {
            Authenticator.authenticate(method,state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testBasicAuthenticationWithNoChallenge() throws Exception {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod();

        assertEquals(false, Authenticator.authenticate(method,state));
    }

    public void testBasicAuthenticationWithNullHttpState() throws Exception {
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm1\""));
        try {
            Authenticator.authenticate(method,(HttpState)null);
            fail("Should have thrown NullPointerException");
        } catch(NullPointerException e) {
            // expected
        }
    }

    public void testInvalidAuthenticationScheme() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials(null,new UsernamePasswordCredentials("username","password"));
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","invalid realm=\"realm1\""));
        try{
            assertTrue(Authenticator.authenticate(method, state));
            fail("Should have thrown UnsupportedOperationException");
        }catch (UnsupportedOperationException uoe){
            // expected
        }
    }

    public void testBasicAuthenticationCaseInsensitivity() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials(null,new UsernamePasswordCredentials("username","password"));
        HttpMethod method = new SimpleHttpMethod(new Header("WwW-AuThEnTiCaTe","bAsIc ReAlM=\"realm1\""));
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        String expected = "Basic " + HttpConstants.getString(Base64.encode(HttpConstants.getBytes("username:password")));
        assertEquals(expected,method.getRequestHeader("Authorization").getValue());
    }


    public void testBasicAuthenticationWithDefaultCreds() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials(null,new UsernamePasswordCredentials("username","password"));
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm1\""));
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        String expected = "Basic " + HttpConstants.getString(Base64.encode(HttpConstants.getBytes("username:password")));
        assertEquals(expected,method.getRequestHeader("Authorization").getValue());
    }

    public void testBasicAuthentication() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials("realm", new UsernamePasswordCredentials("username","password"));
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm\""));
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        String expected = "Basic " + HttpConstants.getString(Base64.encode(HttpConstants.getBytes("username:password")));
        assertEquals(expected,method.getRequestHeader("Authorization").getValue());
    }


    public void testBasicAuthenticationWithMutlipleRealms() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials("realm1",new UsernamePasswordCredentials("username","password"));
        state.setCredentials("realm2",new UsernamePasswordCredentials("uname2","password2"));
        {
            HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm1\""));
            assertTrue(Authenticator.authenticate(method,state));
            assertTrue(null != method.getRequestHeader("Authorization"));
            String expected = "Basic " + HttpConstants.getString(Base64.encode(HttpConstants.getBytes("username:password")));
            assertEquals(expected,method.getRequestHeader("Authorization").getValue());
        }
        {
            HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm2\""));
            assertTrue(Authenticator.authenticate(method,state));
            assertTrue(null != method.getRequestHeader("Authorization"));
            String expected = "Basic " + HttpConstants.getString(Base64.encode(HttpConstants.getBytes("uname2:password2")));
            assertEquals(expected,method.getRequestHeader("Authorization").getValue());
        }
    }

    public void testPreemptiveAuthorizationDefault() throws Exception {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod();
        state.setCredentials(null, new UsernamePasswordCredentials("username","password"));

        assertTrue(! Authenticator.authenticate(method,state));
        assertTrue(null == method.getRequestHeader("Authorization"));
    }

    public void testPreemptiveAuthorizationTrueNoCreds() throws Exception {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod();

        System.getProperties().setProperty(Authenticator.PREEMPTIVE_PROPERTY, "true");
        assertTrue(! Authenticator.authenticate(method,state));
        assertTrue(null == method.getRequestHeader("Authorization"));
    }

    public void testPreemptiveAuthorizationTrueWithCreds() throws Exception {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod();
        state.setCredentials(null, new UsernamePasswordCredentials("username","password"));

        System.getProperties().setProperty(Authenticator.PREEMPTIVE_PROPERTY, "true");
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        String expected = "Basic " + HttpConstants.getString(Base64.encode(HttpConstants.getBytes("username:password")));
        assertEquals(expected, method.getRequestHeader("Authorization").getValue());
    }

    public void testPreemptiveAuthorizationFalse() throws Exception {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod();

        System.getProperties().setProperty(Authenticator.PREEMPTIVE_PROPERTY, "false");
        state.setCredentials(null, new UsernamePasswordCredentials("username","password"));
        assertTrue(! Authenticator.authenticate(method,state));
        assertTrue(null == method.getRequestHeader("Authorization"));
    }

    // --------------------------------- Test Methods for Digest Authentication

    public void testDigestAuthenticationWithNoCreds() {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Digest realm=\"realm1\""));
        try {
            Authenticator.authenticate(method,state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testDigestAuthenticationWithNoRealm() {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Digest"));
        try {
            Authenticator.authenticate(method,state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testDigestAuthenticationWithNoRealm2() {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Digest "));
        try {
            Authenticator.authenticate(method,state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testDigestAuthenticationWithNullHttpState() throws Exception {
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Digest realm=\"realm1\""));
        try {
            Authenticator.authenticate(method,(HttpState)null);
            fail("Should have thrown NullPointerException");
        } catch(NullPointerException e) {
            // expected
        }
    }

    public void testDigestAuthenticationCaseInsensitivity() throws Exception {
        HttpState state = new HttpState();
        UsernamePasswordCredentials cred = new UsernamePasswordCredentials("username","password");
        state.setCredentials(null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WwW-AuThEnTiCaTe","dIgEsT ReAlM=\"realm1\""));
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
    }


    public void testDigestAuthenticationWithDefaultCreds() throws Exception {
        HttpState state = new HttpState();
        UsernamePasswordCredentials cred = new UsernamePasswordCredentials("username","password");
        state.setCredentials(null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Digest realm=\"realm1\""));
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        checkAuthorization(cred, method.getName(), method.getRequestHeader("Authorization").getValue());
    }

    public void testDigestAuthentication() throws Exception {
        HttpState state = new HttpState();
        UsernamePasswordCredentials cred = new UsernamePasswordCredentials("username","password");
        state.setCredentials(null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Digest realm=\"realm1\""));
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        checkAuthorization(cred, method.getName(), method.getRequestHeader("Authorization").getValue());
    }

    public void testDigestAuthenticationWithMultipleRealms() throws Exception {
        HttpState state = new HttpState();
        UsernamePasswordCredentials cred = new UsernamePasswordCredentials("username","password");
        state.setCredentials("realm1", cred);
        UsernamePasswordCredentials cred2 = new UsernamePasswordCredentials("uname2","password2");
        state.setCredentials("realm2", cred2);
        {
            HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Digest realm=\"realm1\""));
            assertTrue(Authenticator.authenticate(method,state));
            assertTrue(null != method.getRequestHeader("Authorization"));
            checkAuthorization(cred, method.getName(), method.getRequestHeader("Authorization").getValue());
        }
        {
            HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Digest realm=\"realm2\""));
            assertTrue(Authenticator.authenticate(method,state));
            assertTrue(null != method.getRequestHeader("Authorization"));
            checkAuthorization(cred2, method.getName(), method.getRequestHeader("Authorization").getValue());
        }
    }
    
    // --------------------------------- Test Methods for NTLM Authentication

    public void testNTLMAuthenticationWithNoCreds() {
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate",
                    "NTLM"));
        method.addRequestHeader("Host", "host");
        try {
            Authenticator.authenticate(method,state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testNTLMAuthenticationWithNullHttpState() throws Exception {
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate",
                    "NTLM"));
        method.addRequestHeader("Host", "host");
        try {
            Authenticator.authenticate(method,(HttpState)null);
            fail("Should have thrown NullPointerException");
        } catch(NullPointerException e) {
            // expected
        }
    }

    public void testNTLMAuthenticationCaseInsensitivity() throws Exception {
        HttpState state = new HttpState();
        NTCredentials cred = new NTCredentials("username","password", "host",
                "domain");
        state.setCredentials("host", cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WwW-AuThEnTiCaTe",
                    "nTlM"));
        method.addRequestHeader("Host", "host");
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
    }

    public void testNTLMAuthenticationResponse1() throws Exception {
        String expected = "NTLM TlRMTVNTUAABAAAABlIAAAYABgAkAAAABAAEACAAAABIT" +
            "1NURE9NQUlO";
        HttpState state = new HttpState();
        NTCredentials cred = new NTCredentials("username","password", "host",
                "domain");
        state.setCredentials("host", cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate",
                    "NTLM"));
        method.addRequestHeader("Host", "host");
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        assertEquals(expected,
                method.getRequestHeader("Authorization").getValue());
    }
    
    public void testNTLMAuthenticationResponse2() throws Exception {
        String expected = "NTLM TlRMTVNTUAADAAAAGAAYAFIAAAAAAAAAagAAAAYABgB" +
            "AAAAACAAIAEYAAAAEAAQATgAAAAAAAABqAAAABlIAAERPTUFJTlVTRVJOQU1FSE" +
            "9TVAaC+vLxUEHnUtpItj9Dp4kzwQfd61Lztg==";
        HttpState state = new HttpState();
        NTCredentials cred = new NTCredentials("username","password", "host",
                "domain");
        state.setCredentials("host", cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate",
            "NTLM TlRMTVNTUAACAAAACgAKADAAAAAGgoEAPc4kP4LtCV8AAAAAAAAAAJ4AngA" +
            "6AAAASU5UUkFFUEhPWAIAFABJAE4AVABSAEEARQBQAEgATwBYAAEAEgBCAE8AQQB" +
            "SAEQAUgBPAE8ATQAEACgAaQBuAHQAcgBhAGUAcABoAG8AeAAuAGUAcABoAG8AeAA" +
            "uAGMAbwBtAAMAPABCAG8AYQByAGQAcgBvAG8AbQAuAGkAbgB0AHIAYQBlAHAAaAB" +
            "vAHgALgBlAHAAaABvAHgALgBjAG8AbQAAAAAA"));
        method.addRequestHeader("Host", "host");
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        assertEquals(expected,
                method.getRequestHeader("Authorization").getValue());
    }
    
    public void testNTLMAuthenticationWithDefaultCreds() throws Exception {
        String expected = "NTLM TlRMTVNTUAADAAAAGAAYAFIAAAAAAAAAagAAAAYABgB" +
            "AAAAACAAIAEYAAAAEAAQATgAAAAAAAABqAAAABlIAAERPTUFJTlVTRVJOQU1FSE" +
            "9TVAaC+vLxUEHnUtpItj9Dp4kzwQfd61Lztg==";
        HttpState state = new HttpState();
        NTCredentials cred = new NTCredentials("username","password", "host",
                "domain");
        state.setCredentials(null, cred);
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate",
            "NTLM TlRMTVNTUAACAAAACgAKADAAAAAGgoEAPc4kP4LtCV8AAAAAAAAAAJ4AngA" +
            "6AAAASU5UUkFFUEhPWAIAFABJAE4AVABSAEEARQBQAEgATwBYAAEAEgBCAE8AQQB" +
            "SAEQAUgBPAE8ATQAEACgAaQBuAHQAcgBhAGUAcABoAG8AeAAuAGUAcABoAG8AeAA" +
            "uAGMAbwBtAAMAPABCAG8AYQByAGQAcgBvAG8AbQAuAGkAbgB0AHIAYQBlAHAAaAB" +
            "vAHgALgBlAHAAaABvAHgALgBjAG8AbQAAAAAA"));
        method.addRequestHeader("Host", "host");
        assertTrue(Authenticator.authenticate(method,state));
        assertTrue(null != method.getRequestHeader("Authorization"));
        assertEquals(expected,
                method.getRequestHeader("Authorization").getValue());
    }

    /** 
     * Test that the Unauthorized response is returned when doAuthentication is false.
     */
    public void testDoAuthenticateFalse() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials("Protected",
                new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        method.setDoAuthentication(false);
        SimpleHttpConnection conn = new SimpleHttpConnection();
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" + 
            "WWW-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n");
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"); 
        method.execute(state, conn);
        assertNotNull(method.getResponseHeader("WWW-Authenticate"));
        assertNull(method.getRequestHeader("Authorization"));
        assertEquals(401, method.getStatusCode());

    }


    /** 
     */
    public void testInvalidCredentials() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials("Protected", new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        method.setDoAuthentication(false);
        SimpleHttpConnection conn = new SimpleHttpConnection();
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" + 
            "WWW-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        method.execute(state, conn);
        assertEquals(401, method.getStatusCode());
    }


    // --------------------------------- Test Methods for Multiple Authentication

    public void testMultipleChallengeBasic() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials("Protected", new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        SimpleHttpConnection conn = new SimpleHttpConnection();
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
        method.execute(state, conn);
        Header authHeader = method.getRequestHeader("Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Basic"));
    }

    public void testMultipleChallengeBasicLongRealm() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials(null, new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        SimpleHttpConnection conn = new SimpleHttpConnection();
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
        method.execute(state, conn);
        Header authHeader = method.getRequestHeader("Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Basic"));
    }




    public void testMultipleChallengeDigest() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials("Protected", new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        SimpleHttpConnection conn = new SimpleHttpConnection();
        conn.addResponse(
            "HTTP/1.1 401 Unauthorized\r\n" + 
            "WWW-Authenticate: Unsupported\r\n" +
            "WWW-Authenticate: Digest realm=\"Protected\"\r\n" +
            "WWW-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                ); 
        method.execute(state, conn);
        Header authHeader = method.getRequestHeader("Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Digest"));
    }


    public void testMultipleProxyChallengeBasic() throws Exception {
        HttpState state = new HttpState();
        state.setProxyCredentials("Protected", new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        SimpleHttpConnection conn = new SimpleHttpConnection();
        conn.addResponse(
            "HTTP/1.1 407 Proxy Authentication Required\r\n" + 
            "Proxy-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Proxy-Authenticate: Unsupported\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                ); 
        method.execute(state, conn);
        Header authHeader = method.getRequestHeader("Proxy-Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Basic"));
    }


    public void testMultipleProxyChallengeDigest() throws Exception {
        HttpState state = new HttpState();
        state.setProxyCredentials("Protected", new UsernamePasswordCredentials("name", "pass"));
        HttpMethod method = new SimpleHttpMethod();
        SimpleHttpConnection conn = new SimpleHttpConnection();
        conn.addResponse(
            "HTTP/1.1 407 Proxy Authentication Required\r\n" + 
            "Proxy-Authenticate: Basic realm=\"Protected\"\r\n" +
            "Proxy-Authenticate: Digest realm=\"Protected\"\r\n" +
            "Proxy-Authenticate: Unsupported\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                );
        conn.addResponse( 
            "HTTP/1.1 200 OK\r\n" +
            "Connection: close\r\n" +
            "Server: HttpClient Test/2.0\r\n"
                ); 
        method.execute(state, conn);
        Header authHeader = method.getRequestHeader("Proxy-Authorization");
        assertNotNull(authHeader);

        String authValue = authHeader.getValue();
        assertTrue(authValue.startsWith("Digest"));
    }



}
