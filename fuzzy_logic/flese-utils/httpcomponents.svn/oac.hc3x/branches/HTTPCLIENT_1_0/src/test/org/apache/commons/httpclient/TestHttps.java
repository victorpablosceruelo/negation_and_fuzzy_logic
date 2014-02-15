/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestHttps.java,v 1.2 2001/07/27 22:52:25 rwaldhoff Exp $
 * $Revision: 1.2 $
 * $Date: 2001-07-28 00:52:25 +0200 (Sat, 28 Jul 2001) $
 * ====================================================================
 * Copyright (C) The Apache Software Foundation. All rights reserved.
 *
 * This software is published under the terms of the Apache Software License
 * version 1.1, a copy of which has been included with this distribution in
 * the LICENSE file.
 */

package org.apache.commons.httpclient;

import java.io.IOException;
import java.net.URL;
import junit.framework.*;
import org.apache.commons.httpclient.methods.*;

/**
 * Simple tests for HTTPS support in HttpClient.
 *
 * To run this test you'll need:
 *  + a JSSE implementation installed (see README.txt)
 *  + the java.protocol.handler.pkgs system property set
 *    for your provider.  e.g.:
 *     -Djava.protocol.handler.pkgs=com.sun.net.ssl.internal.www.protocol
 *    (see build.xml)
 *
 * @author Rodney Waldhoff
 * @version $Id: TestHttps.java 133467 2001-07-27 22:52:25Z rwaldhoff $
 */
public class TestHttps extends TestCase {

    // ---------------------------------------------------------------- Members
    private URL _urlWithPort = null;
    private URL _urlWithoutPort = null;

    // ------------------------------------------------------------ Constructor
    public TestHttps(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestHttps.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods
    public static Test suite() {
        return new TestSuite(TestHttps.class);
    }

    public void setUp() throws Exception {
        _urlWithPort = new URL("https://www.verisign.com:443/");
        _urlWithoutPort = new URL("https://www.verisign.com/");
    }

    public void testHttpsGet() {
        HttpClient client = new HttpClient();
        client.startSession(_urlWithPort);
        GetMethod method = new GetMethod("/");
        method.setUseDisk(false);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Exception thrown during HTTPS GET: " + t.toString());
        }

        try {
            String data = method.getDataAsString();
            // This enumeration musn't be empty
            assert("No data returned.", (data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Exception thrown while retrieving data : " + t.toString());
        }
    }

    public void testHttpsGetNoPort() {
        HttpClient client = new HttpClient();
        client.startSession(_urlWithoutPort);
        GetMethod method = new GetMethod("/");
        method.setUseDisk(false);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Exception thrown during HTTPS GET: " + t.toString());
        }

        try {
            String data = method.getDataAsString();
            // This enumeration musn't be empty
            assert("No data returned.", (data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Exception thrown while retrieving data : " + t.toString());
        }
    }
}
