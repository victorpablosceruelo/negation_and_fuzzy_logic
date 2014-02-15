/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestGetMethodLocal.java,v 1.14 2004/02/22 18:08:49 olegk Exp $
 * $Revision: 1.14 $
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

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.GetMethod;

/**
 * Simple tests of {@link GetMethod}.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestGetMethodLocal.java 134530 2004-02-22 18:08:52Z olegk $
 */
public class TestGetMethodLocal extends TestLocalHostBase {

    // ------------------------------------------------------------ Constructor

    public TestGetMethodLocal(String testName) {
        super(testName);
    }


    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestGetMethodLocal.class);
    }

    // ------------------------------------------------------------------ Tests

    public void testGetSlash() {
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
            assertTrue("No data returned.",(data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
    }

    public void testExecuteMultipleMethods() throws Exception {

        HttpClient client = createHttpClient();

        GetMethod getSlash = new GetMethod("/");
        for(int i=0;i<10;i++) {
            assertEquals(200, client.executeMethod(getSlash));
            String data = getSlash.getResponseBodyAsString();
            assertTrue(null != data);
            assertTrue(data.length() > 0);
            getSlash.recycle();
            getSlash.setPath("/");
        }
    }

    public void testRecycle() {
        HttpClient client = createHttpClient(null);

        GetMethod method = new GetMethod("/");
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            assertTrue("No data returned.",(data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());

        method.recycle();
        method.setPath("/");

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            assertTrue("No data returned.",(data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());

    }

    public void test404() {
        HttpClient client = createHttpClient(null);

        GetMethod method = new GetMethod("/i/am/assuming/this/path/and/file/doesnt/exist/on/the/web/server.xyzzy");
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(404,method.getStatusCode());

    }

    /**
     * The intent of this test is to allow for the incomplete parsing of a GET
     * response, and to make it particularly tricky, the GET response issues
     * a Connection: close".
     *
     * <p>This wants to insure that a recoverable exception is not unexpectedly
     * triggered.</p>
     */
    public void testGetResponseNotReadAutoRecover() {
        HttpClient client = createHttpClient(null);

        try {
            // issue a GET with a connection: close, and don't parse the body.
            String path = "/";
            GetMethod method1 = new GetMethod(path);
            method1.addRequestHeader("Connection", "close");
            client.executeMethod(method1);

            // issue another GET.
            GetMethod method2 = new GetMethod(path);
            client.executeMethod(method2);
        }
        catch (Exception ioe) {

            fail("Problem executing method : " + ioe.toString() );
        }
    }

}
