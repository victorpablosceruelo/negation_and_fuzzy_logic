/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestAuthenticator.java,v 1.4 2001/08/08 19:01:18 rwaldhoff Exp $
 * $Revision: 1.4 $
 * $Date: 2001-08-08 21:01:18 +0200 (Wed, 08 Aug 2001) $
 * ====================================================================
 * Copyright (C) The Apache Software Foundation. All rights reserved.
 *
 * This software is published under the terms of the Apache Software License
 * version 1.1, a copy of which has been included with this distribution in
 * the LICENSE file.
 */

package org.apache.commons.httpclient;

import junit.framework.*;

/**
 * Unit tests for {@link Authenticator}.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestAuthenticator.java 133485 2001-08-08 19:01:18Z rwaldhoff $
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

    // ----------------------------------------------------------- Test Methods

    public void testBasicAuthenticationWithNoCreds() {
        State state = new State();
        try {
            Authenticator.challengeResponse("Basic realm=\"realm1\"",state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testBasicAuthenticationWithNoRealm() {
        State state = new State();
        try {
            Authenticator.challengeResponse("Basic",state);
            fail("Should have thrown HttpException");
        } catch(HttpException e) {
            // expected
        }
    }

    public void testBasicAuthenticationWithNoChallenge() throws Exception {
        State state = new State();
        assert(null == Authenticator.challengeResponse((String)null,state));
    }

    public void testBasicAuthenticationWithNullState() throws Exception {
        try {
            Authenticator.challengeResponse("Basic realm=\"realm1\"",(State)null);
            fail("Should have thrown NullPointerException");
        } catch(NullPointerException e) {
            // expected
        }
    }

    public void testBasicAuthenticationWithDefaultCreds() throws Exception {
        State state = new State();
        state.setDefaultCredentials(new Credentials("username","password"));
        String response = Authenticator.challengeResponse("Basic realm=\"realm1\"",state);
        String expected = "Basic " + new String(Base64.encode("username:password".getBytes()));
        assertEquals(expected,response);
    }

    public void testBasicAuthentication() throws Exception {
        State state = new State();
        state.setCredentials("realm1",new Credentials("username","password"));
        String response = Authenticator.challengeResponse("Basic realm=\"realm1\"",state);
        String expected = "Basic " + new String(Base64.encode("username:password".getBytes()));
        assertEquals(expected,response);
    }

    public void testBasicAuthenticationWithMutlipleRealms() throws Exception {
        State state = new State();
        state.setCredentials("realm1",new Credentials("username","password"));
        state.setCredentials("realm2",new Credentials("uname2","password2"));
        {
            String response = Authenticator.challengeResponse("Basic realm=\"realm1\"",state);
            String expected = "Basic " + new String(Base64.encode("username:password".getBytes()));
            assertEquals(expected,response);
        }
        {
            String response = Authenticator.challengeResponse("Basic realm=\"realm2\"",state);
            String expected = "Basic " + new String(Base64.encode("uname2:password2".getBytes()));
            assertEquals(expected,response);
        }
    }

}
