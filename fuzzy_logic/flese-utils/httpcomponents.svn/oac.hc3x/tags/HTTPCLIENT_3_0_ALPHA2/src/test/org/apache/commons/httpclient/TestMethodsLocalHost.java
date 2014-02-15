/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestMethodsLocalHost.java,v 1.14 2004/06/13 20:22:19 olegk Exp $
 * $Revision: 1.14 $
 * $Date: 2004-06-13 22:22:20 +0200 (Sun, 13 Jun 2004) $
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

import java.util.Enumeration;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.HeadMethod;
import org.apache.commons.httpclient.methods.OptionsMethod;

/**
 * Simple tests for the HTTP client hitting a local webserver.
 *
 * This test assumes a webserver is running on port 8080 on
 * the 127.0.0.1.
 *
 * The default configuration of Tomcat 4 will work fine.
 *
 * Tomcat 3.x will fail the OPTIONS test, because it
 * treats OPTIONS as a GET request.
 *
 * @author Remy Maucherat
 * @author Rodney Waldhoff
 * @version $Id: TestMethodsLocalHost.java 134635 2004-06-13 20:22:20Z olegk $
 */
public class TestMethodsLocalHost extends TestLocalHostBase {

    // ------------------------------------------------------------ Constructor

    public TestMethodsLocalHost(String testName) {
        super(testName);
    }


    // ------------------------------------------------------- TestCase Methods


    public static Test suite() {
        return new TestSuite(TestMethodsLocalHost.class);
    }


    // ----------------------------------------------------------- OPTIONS Test

    /**
     * This test assumes that the webserver listening
     * on host/port will respond properly to an OPTIONS
     * request.  Tomcat 4 is one such web server,
     * but Tomcat 3.x is not.
     */
    public void testMethodsOptions() {

        HttpClient client = createHttpClient();
        OptionsMethod method = new OptionsMethod("/");

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        Enumeration methodsAllowed = method.getAllowedMethods();
        // This enumeration musn't be empty
        assertTrue("No HTTP method allowed : result of OPTIONS is incorrect "
               + "(make sure the webserver running on port " + getPort()
               + " supports OPTIONS properly)",
               methodsAllowed.hasMoreElements());

    }


    // --------------------------------------------------------------- GET Test


    public void testMethodsGet() {

        HttpClient client = createHttpClient();

        GetMethod method = new GetMethod("/");
        

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            // This enumeration musn't be empty
            assertTrue("No data returned.",
                   (data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        method = new GetMethod("/index.html");

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

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


    public void testMethodsHead() {

        HttpClient client = createHttpClient();

        OptionsMethod opmethod = new OptionsMethod("/");

        try {
            client.executeMethod(opmethod);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        String path = "/";
        HeadMethod method = new HeadMethod(path);

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        assertEquals(200, method.getStatusCode());

        method = new HeadMethod(path);

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        assertEquals(200, method.getStatusCode());

    }

}
