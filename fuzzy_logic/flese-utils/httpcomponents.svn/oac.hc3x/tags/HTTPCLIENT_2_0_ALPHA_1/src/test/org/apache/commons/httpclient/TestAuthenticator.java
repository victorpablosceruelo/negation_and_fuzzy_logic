/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestAuthenticator.java,v 1.5 2001/10/04 17:49:13 rwaldhoff Exp $
 * $Revision: 1.5 $
 * $Date: 2001-10-04 19:49:15 +0200 (Thu, 04 Oct 2001) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999 The Apache Software Foundation.  All rights
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

import junit.framework.*;

/**
 * Unit tests for {@link Authenticator}.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestAuthenticator.java 133582 2001-10-04 17:49:15Z rwaldhoff $
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

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestAuthenticator.class);
    }

    // ------------------------------------------------------------ Inner Class

    // HttpMethodBase.responseHeaders is private,
    // so this extension adds a mechanism for setting
    // a response header
    private class SimpleHttpMethod extends HttpMethodBase {
        Header _header = null;
        public SimpleHttpMethod(Header header) {
            _header = header;
        }
        public String getName() { return "simple"; }

        public Header getResponseHeader(String name) {
            try {
                if(name.equalsIgnoreCase(_header.getName())) {
                    return _header;
                } else {
                    return super.getResponseHeader(name);
                }
            } catch(NullPointerException e) {
                return super.getResponseHeader(name);
            }
        }
    }

    // ----------------------------------------------------------- Test Methods

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
        HttpMethod method = new SimpleHttpMethod(null);
        assert(false == Authenticator.authenticate(method,state));
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

    public void testBasicAuthenticationWithDefaultCreds() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials(null,new UsernamePasswordCredentials("username","password"));
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm1\""));
        assert(Authenticator.authenticate(method,state));
        assert(null != method.getRequestHeader("Authorization"));
        String expected = "Basic " + new String(Base64.encode("username:password".getBytes()));
        assertEquals(expected,method.getRequestHeader("Authorization").getValue());
    }

    public void testBasicAuthentication() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials("realm1",new UsernamePasswordCredentials("username","password"));
        HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm1\""));
        assert(Authenticator.authenticate(method,state));
        assert(null != method.getRequestHeader("Authorization"));
        String expected = "Basic " + new String(Base64.encode("username:password".getBytes()));
        assertEquals(expected,method.getRequestHeader("Authorization").getValue());
    }

    public void testBasicAuthenticationWithMutlipleRealms() throws Exception {
        HttpState state = new HttpState();
        state.setCredentials("realm1",new UsernamePasswordCredentials("username","password"));
        state.setCredentials("realm2",new UsernamePasswordCredentials("uname2","password2"));
        {
            HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm1\""));
            assert(Authenticator.authenticate(method,state));
            assert(null != method.getRequestHeader("Authorization"));
            String expected = "Basic " + new String(Base64.encode("username:password".getBytes()));
            assertEquals(expected,method.getRequestHeader("Authorization").getValue());
        }
        {
            HttpMethod method = new SimpleHttpMethod(new Header("WWW-Authenticate","Basic realm=\"realm2\""));
            assert(Authenticator.authenticate(method,state));
            assert(null != method.getRequestHeader("Authorization"));
            String expected = "Basic " + new String(Base64.encode("uname2:password2".getBytes()));
            assertEquals(expected,method.getRequestHeader("Authorization").getValue());
        }
    }
}
