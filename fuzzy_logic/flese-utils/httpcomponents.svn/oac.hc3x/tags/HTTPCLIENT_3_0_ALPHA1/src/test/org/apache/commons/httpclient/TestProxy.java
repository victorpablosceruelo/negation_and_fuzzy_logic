/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestProxy.java,v 1.5 2004/02/22 18:08:49 olegk Exp $
 * $Revision: 1.5 $
 * $Date: 2004-02-22 19:08:52 +0100 (Sun, 22 Feb 2004) $
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

import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.httpclient.server.SimpleProxy;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Tests for proxied connections.
 * 
 * @author Ortwin Glueck
 */
public class TestProxy extends TestCase {

    private static String TARGET_HOST = null;
    private static int TARGET_PORT = -1;

    public TestProxy(String testName) {
        super(testName);
    }

    public static Test suite() {
        return new TestSuite(TestProxy.class);
    }

    protected void setUp() throws Exception {
        super.setUp();
        TARGET_HOST = System.getProperty("httpclient.test.localHost", "localhost");
        TARGET_PORT = Integer.parseInt(System.getProperty("httpclient.test.localPort", "8080")); 
    }

    public void testSimpleGet() throws Exception {
        SimpleProxy proxy = new SimpleProxy();

        HttpClient client = new HttpClient();
        HostConfiguration hc = new HostConfiguration();
        hc.setHost(TARGET_HOST, TARGET_PORT, Protocol.getProtocol("http"));
        hc.setProxy(proxy.getLocalAddress(), proxy.getLocalPort());
        client.setHostConfiguration(hc);

        GetMethod get = new GetMethod("/");
        client.executeMethod(get);
        assertEquals(200, get.getStatusCode());
        get.releaseConnection();
    }

    public void testAuthGet() throws Exception {
        Credentials creds = new UsernamePasswordCredentials("user", "password");
        SimpleProxy proxy = new SimpleProxy();
        proxy.requireCredentials(creds);

        HttpClient client = new HttpClient();
        HostConfiguration hc = new HostConfiguration();
        hc.setHost(TARGET_HOST, TARGET_PORT, Protocol.getProtocol("http"));
        hc.setProxy(proxy.getLocalAddress(), proxy.getLocalPort());
        client.setHostConfiguration(hc);
        client.getState().setProxyCredentials(null, null, creds);

        GetMethod get = new GetMethod("/");
        client.executeMethod(get);
        assertEquals(200, get.getStatusCode());
        get.releaseConnection();
    }

}
