/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestWebappMethods.java,v 1.22 2004/05/12 20:43:54 olegk Exp $
 * $Revision: 1.22 $
 * $Date: 2004-05-12 22:43:54 +0200 (Wed, 12 May 2004) $
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

import junit.framework.*;
import org.apache.commons.httpclient.methods.*;
import org.apache.commons.httpclient.util.EncodingUtil;

/**
 * This suite of tests depends upon the httpclienttest webapp,
 * which is available in the httpclient/src/test-webapp
 * directory in the CVS tree.
 * <p>
 * The webapp should be deployed in the context "httpclienttest"
 * on a servlet engine running on port 8080 on the localhost
 * (IP 127.0.0.1).
 * <p>
 * You can change the assumed port by setting the
 * "httpclient.test.localPort" property.
 * You can change the assumed host by setting the
 * "httpclient.test.localHost" property.
 * You can change the assumed context by setting the
 * "httpclient.test.webappContext" property.
 *
 * @author Rodney Waldhoff
 * @author Ortwin Glück
 * @version $Id: TestWebappMethods.java 134607 2004-05-12 20:43:54Z olegk $
 */
public class TestWebappMethods extends TestWebappBase {

    public TestWebappMethods(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestWebappMethods.class);
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestWebappMethods.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------------------ Tests

    /**
     * Simple test of {@link GetMethod} against /httpclienttest/params.
     */
    public void testGetMethod() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod("/" + getWebappContext() + "/params");
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertEquals(200,method.getStatusCode());

        method.recycle();

        method.setPath("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertEquals(200,method.getStatusCode());
    }

    /**
     * Simple test of {@link PostMethod} against /httpclienttest/params.
     */
    public void testPostMethod() throws Exception {
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod("/" + getWebappContext() + "/params");
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: POST</title>") >= 0);
        assertEquals(200,method.getStatusCode());

        method.recycle();

        method.setPath("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: POST</title>") >= 0);
        assertEquals(200,method.getStatusCode());
    }

    /**
     * Simple test of {@link HeadMethod} against /httpclienttest/params.
     */
    public void testHeadMethod() throws Exception {
        HttpClient client = createHttpClient();
        HeadMethod method = new HeadMethod("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());

        method.recycle();

        method.setPath("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
    }

    /**
     * Simple test of {@link OptionsMethod} against /httpclienttest/params.
     */
    public void testOptionsMethod() throws Exception {
        HttpClient client = createHttpClient();
        OptionsMethod method = new OptionsMethod("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getAllowedMethods().hasMoreElements());

        method.recycle();

        method.setPath("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getAllowedMethods().hasMoreElements());
    }

    /**
     * Simple test of {@link OptionsMethod} against the path "*".
     */
    public void testOptionsStar() throws Exception {
        HttpClient client = createHttpClient();
        OptionsMethod method = new OptionsMethod("*");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getAllowedMethods().hasMoreElements());
    }

    /**
     * Simple test of {@link DeleteMethod} against /httpclienttest/params.
     */
    public void testDeleteMethod() throws Exception {
        HttpClient client = createHttpClient();
        DeleteMethod method = new DeleteMethod("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());

        method.recycle();

        method.setPath("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
    }

    /**
     * Simple test of {@link PutMethod} against /httpclienttest/params.
     */
    public void testPutMethod() throws Exception {
        HttpClient client = createHttpClient();
        PutMethod method = new PutMethod("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString(),method.getResponseBodyAsString().indexOf("<title>Param Servlet: PUT</title>") >= 0);

        method.recycle();

        method.setPath("/" + getWebappContext() + "/params");
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString(),method.getResponseBodyAsString().indexOf("<title>Param Servlet: PUT</title>") >= 0);
    }

    public void testPostBodyNVP() throws Exception {
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod("/" + getWebappContext() + "/body");
        
        method.setRequestBody(new NameValuePair[] { 
           new NameValuePair("quote","It was the best of times, it was the worst of times.") } );
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<tt>quote=It+was+the+best+of+times%2C+it+was+the+worst+of+times.</tt>") >= 0);
    }

    public void testPostBody() throws Exception {
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod("/" + getWebappContext() + "/body");
        
        method.setRequestEntity(new StringRequestEntity("quote=It+was+the+best+of+times%2C+it+was+the+worst+of+times."));
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<tt>quote=It+was+the+best+of+times%2C+it+was+the+worst+of+times.</tt>") >= 0);
        assertEquals(200,method.getStatusCode());
    }


    public void testPostBodyCustomLength() throws Exception {
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod("/" + getWebappContext() + "/body");
        
        String bodyStr = "quote=It+was+the+best+of+times%2C+it+was+the+worst+of+times.";
		byte[] body = EncodingUtil.getBytes(bodyStr, "ISO-8859-1");

        method.setRequestEntity(new ByteArrayRequestEntity(body));
        client.executeMethod(method);
        assertTrue(method.getResponseBodyAsString().indexOf("<tt>quote=It+was+the+best+of+times%2C+it+was+the+worst+of+times.</tt>") >= 0);
        assertEquals(200,method.getStatusCode());
    }


    public void testPostBodyAutoLength() throws Exception {
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod("/" + getWebappContext() + "/body");
        
        String body = "quote=It+was+the+best+of+times%2C+it+was+the+worst+of+times.";
        method.setRequestEntity(new StringRequestEntity(body));
        client.executeMethod(method);
        assertTrue(method.getResponseBodyAsString().indexOf("<tt>quote=It+was+the+best+of+times%2C+it+was+the+worst+of+times.</tt>") >= 0);
        assertEquals(200,method.getStatusCode());
    }


    public void testPostBodyChunked() throws Exception  {
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod("/" + getWebappContext() + "/body");
        
        String body = "quote=It+was+the+best+of+times%2C+it+was+the+worst+of+times.";
        method.setRequestEntity(new StringRequestEntity(body));
        method.setContentChunked(true);
        client.executeMethod(method);
        assertTrue(method.getResponseBodyAsString().indexOf("<tt>quote=It+was+the+best+of+times%2C+it+was+the+worst+of+times.</tt>") >= 0);
        assertEquals(200,method.getStatusLine().getStatusCode());
    }


    public void testPutBody() throws Exception {
        HttpClient client = createHttpClient();
        PutMethod method = new PutMethod("/" + getWebappContext() + "/body");
        method.setRequestEntity(new StringRequestEntity("This is data to be sent in the body of an HTTP PUT."));
        client.executeMethod(method);
        assertTrue(method.getResponseBodyAsString(),method.getResponseBodyAsString().indexOf("<tt>This is data to be sent in the body of an HTTP PUT.</tt>") >= 0);
        assertEquals(200,method.getStatusCode());
    }


    public void testPostMethodRecycle() throws Exception {
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod("/" + getWebappContext() + "/body");
        
        String bodyStr = "Like, hello, and stuff";
        byte [] body = EncodingUtil.getBytes(bodyStr, "ISO-8859-1");
        method.setRequestHeader("Content-Type", "text/plain");
        method.setRequestEntity(new ByteArrayRequestEntity(body));
        client.executeMethod(method);
        assertEquals(200,method.getStatusLine().getStatusCode());
        String response = method.getResponseBodyAsString();

        method.recycle();

        method.setPath("/" + getWebappContext() + "/body");
        method.setRequestHeader("Content-Type", "text/plain");
        method.setRequestEntity(new ByteArrayRequestEntity(body));
        client.executeMethod(method);
        assertEquals(200,method.getStatusLine().getStatusCode());
        response = method.getResponseBodyAsString();
    }

    public void testEmptyPostMethod() throws Exception {
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod("/" + getWebappContext() + "/body");
        
        method.setRequestHeader("Content-Type", "text/plain");
        client.executeMethod(method);
        assertEquals(200,method.getStatusLine().getStatusCode());
        String response = method.getResponseBodyAsString();
        assertTrue(response.indexOf("No body submitted") >= 0);

        method.recycle();

        method.setPath("/" + getWebappContext() + "/body");
        method.setRequestHeader("Content-Type", "text/plain");
        client.executeMethod(method);
        assertEquals(200,method.getStatusLine().getStatusCode());
        response = method.getResponseBodyAsString();
        assertTrue(response.indexOf("No body submitted") >= 0);

        method.recycle();

        method.setPath("/" + getWebappContext() + "/body");
        method.setRequestHeader("Content-Type", "text/plain");
        method.setRequestEntity(new StringRequestEntity(""));
        client.executeMethod(method);
        assertEquals(200,method.getStatusLine().getStatusCode());
        response = method.getResponseBodyAsString();
        assertTrue(response.indexOf("No body submitted") >= 0);

        method.recycle();

        method.setPath("/" + getWebappContext() + "/body");
        method.setRequestHeader("Content-Type", "text/plain");
        method.setContentChunked(true);
        client.executeMethod(method);
        assertEquals(200,method.getStatusLine().getStatusCode());
        response = method.getResponseBodyAsString();
        assertTrue(response.indexOf("No body submitted") >= 0);

        method.recycle();

        method.setPath("/" + getWebappContext() + "/body");
        method.setRequestHeader("Content-Type", "text/plain");
        method.setRequestEntity(new StringRequestEntity(""));
        method.setContentChunked(true);
        client.executeMethod(method);
        assertEquals(200,method.getStatusLine().getStatusCode());
        response = method.getResponseBodyAsString();

    }

}
