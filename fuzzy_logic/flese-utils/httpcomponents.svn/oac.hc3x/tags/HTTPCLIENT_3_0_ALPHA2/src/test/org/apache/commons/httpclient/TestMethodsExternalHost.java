/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestMethodsExternalHost.java,v 1.15 2004/09/17 07:57:49 oglueck Exp $
 * $Revision: 1.15 $
 * $Date: 2004-09-17 09:57:49 +0200 (Fri, 17 Sep 2004) $
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

import java.io.IOException;
import java.util.Enumeration;
import junit.framework.*;

import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.*;

/**
 * Simple tests for the HTTP client hitting an external webserver.
 *
 * This test suite assumes you have an internet connection that
 * can communicate with http://java.sun.com/.
 *
 * @author Remy Maucherat
 * @author Rodney Waldhoff
 * @author Ortwin Glück
 * @author Jeff Dever
 * @version $Id: TestMethodsExternalHost.java 134699 2004-09-17 07:57:49Z oglueck $
 */
public class TestMethodsExternalHost extends TestCase {

    private HttpClient client;
    private HttpMethod method;

    // -------------------------------------------------------------- Constants

    private static final String externalHost = "jakarta.apache.org";
    private static final int externalPort = 80;
    private static final String externalPath = "/index.html";
    private static final String externalUri = "http://"+ externalHost + externalPath;
    private final String PROXY_HOST = System.getProperty("httpclient.test.proxyHost");
    private final String PROXY_PORT = System.getProperty("httpclient.test.proxyPort");
    private final String PROXY_USER = System.getProperty("httpclient.test.proxyUser");
    private final String PROXY_PASS = System.getProperty("httpclient.test.proxyPass");

    // ------------------------------------------------------------ Constructor


    public TestMethodsExternalHost(String testName) {
        super(testName);
    }


    // ------------------------------------------------------- TestCase Methods


    public static Test suite() {
        return new TestSuite(TestMethodsExternalHost.class);
    }

    // ------------------------------------------------------- Helper Methods
    
    public void setUp() {
        client = new HttpClient();

	    client.getHostConfiguration().setHost(externalHost, externalPort, "http");

        if (PROXY_HOST != null) {
            if (PROXY_USER != null) {
                HttpState state = client.getState();
                state.setProxyCredentials(AuthScope.ANY, new UsernamePasswordCredentials(
                    PROXY_USER, PROXY_PASS));
            }
            client.getHostConfiguration().setProxy(PROXY_HOST, Integer.parseInt(PROXY_PORT));
        }
    }

    public void tearDown() {
        method.releaseConnection();
        method = null;
        client = null;
    }

    public void executeMethod() {
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
    }

    // ----------------------------------------------------------- OPTIONS Test


    public void testMethodsOptionsExternal() {

        method = new OptionsMethod(externalPath);
        executeMethod();

        Enumeration methodsAllowed = ((OptionsMethod)method).getAllowedMethods();
        // This enumeration musn't be empty
        assertTrue("No HTTP method allowed : result of OPTIONS is incorrect.",
               methodsAllowed.hasMoreElements());

    }
    // --------------------------------------------------------------- GET Test


    public void testMethodsGetExternal() {

        method = new GetMethod(externalUri);
        executeMethod();

        try {
            String data = method.getResponseBodyAsString();
            // This enumeration musn't be empty
            assertTrue("No data returned.",
                   (data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        method = new GetMethod(externalUri);
        executeMethod();

        try {
            String data = method.getResponseBodyAsString();
            // This enumeration musn't be empty
            assertTrue("No data returned.",
                   (data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

    }


    // -------------------------------------------------------------- HEAD Test

    public void testMethodsHeadExternal() {

        method = new HeadMethod(externalPath);
        executeMethod();

        assertTrue("Method failed : " + method.getStatusCode(),
               (method.getStatusCode() == 200));

    }

    /**
     * This test proves that bad urls throw an IOException,
     * and not some other throwable like a NullPointerException.
     *
     * FIXME: Bad urls don't throw an IOException.
     */
    public void testIOException() {

        method = new GetMethod("http://www.bogusurl.xyz");

        try {
            client.executeMethod(method);
            if ((PROXY_HOST != null) && (method.getStatusCode() >= 400)) return;
        } catch (IOException e) {
            return; // IOException and HttpException are ok
        }
        fail("Should have thrown an exception");

    }


    /**
     * http://nagoya.apache.org/bugzilla/show_bug.cgi?id=16864
     */
    public void testDomino_Go_Webserver404() throws Exception {

        // this file should not exist
        method = new GetMethod("http://www.pc.ibm.com/us/accessories/monitors/p_allmodelos.html");
        client.executeMethod(method);

        assertEquals(404, method.getStatusCode());

    }


}
