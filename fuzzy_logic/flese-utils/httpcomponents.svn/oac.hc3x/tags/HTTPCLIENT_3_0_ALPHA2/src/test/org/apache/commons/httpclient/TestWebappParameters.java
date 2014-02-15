/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestWebappParameters.java,v 1.11 2004/02/22 18:08:50 olegk Exp $
 * $Revision: 1.11 $
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

import junit.framework.*;
import org.apache.commons.httpclient.methods.*;

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
 * @version $Id: TestWebappParameters.java 134530 2004-02-22 18:08:52Z olegk $
 */
public class TestWebappParameters extends TestWebappBase {

    public TestWebappParameters(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestWebappParameters.class);
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestWebappParameters.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------------------ Tests

    /**
     * Test that {@link GetMethod#setQueryString(java.lang.String)}
     * can include a leading question mark.
     */
    public void testGetMethodQueryString() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod("/" + getWebappContext() + "/params");
        method.setQueryString("?hadQuestionMark=true");
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<p>QueryString=\"hadQuestionMark=true\"</p>") >= 0);
    }

    /**
     * Test that {@link GetMethod#setQueryString(java.lang.String)}
     * doesn't have to include a leading question mark.
     */
    public void testGetMethodQueryString2() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod("/" + getWebappContext() + "/params");
        method.setQueryString("hadQuestionMark=false");
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<p>QueryString=\"hadQuestionMark=false\"</p>") >= 0);
    }

    /**
     * Test that {@link GetMethod#addParameter(java.lang.String,java.lang.String)}
     * values get added to the query string.
     */
    public void testGetMethodParameters() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod("/" + getWebappContext() + "/params");
        method.setQueryString(new NameValuePair[] { new NameValuePair("param-one","param-value") });
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<p>QueryString=\"param-one=param-value\"</p>") >= 0);
    }

    /**
     * Test that {@link GetMethod#addParameter(java.lang.String,java.lang.String)}
     * works with multiple parameters.
     */
    public void testGetMethodMultiParameters() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod("/" + getWebappContext() + "/params");
        method.setQueryString(new NameValuePair[] {
                                new NameValuePair("param-one","param-value"),
                                new NameValuePair("param-two","param-value2"),
                                new NameValuePair("special-chars",":/?~.")
                              });
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"special-chars\";value=\":/?~.\"") >= 0);
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"param-one\";value=\"param-value\"") >= 0);
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"param-two\";value=\"param-value2\"") >= 0);
    }

    /**
     * Test that {@link GetMethod#addParameter(java.lang.String,java.lang.String)}
     * works with a parameter name but no value.
     */
    public void testGetMethodParameterWithoutValue() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod("/" + getWebappContext() + "/params");
        method.setQueryString(new NameValuePair[] { new NameValuePair("param-without-value",null) });
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<p>QueryString=\"param-without-value=\"</p>") >= 0);
    }

    /**
     * Test that {@link GetMethod#addParameter(java.lang.String,java.lang.String)}
     * works with a parameter name that occurs more than once.
     */
    public void testGetMethodParameterAppearsTwice() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod("/" + getWebappContext() + "/params");
        method.setQueryString(new NameValuePair[] {
                                  new NameValuePair("foo","one"),
                                  new NameValuePair("foo","two")
                             });
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"foo\";value=\"one\"") >= 0);
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"foo\";value=\"two\"") >= 0);
    }

    public void testGetMethodOverwriteQueryString() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod("/" + getWebappContext() + "/params");
        method.setQueryString("query=string");
        method.setQueryString(new NameValuePair[] {
                                  new NameValuePair("param","eter"),
                                  new NameValuePair("para","meter")
                             });
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"query\";value=\"string\"") == -1);
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"param\";value=\"eter\"") >= 0);
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"para\";value=\"meter\"") >= 0);
    }

    /**
     * Test that {@link PostMethod#addParameter(java.lang.String,java.lang.String)}
     * and {@link PostMethod#setQueryString(java.lang.String)} combine
     * properly.
     */
    public void testPostMethodParameterAndQueryString() throws Exception {
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod("/" + getWebappContext() + "/params");
        method.setQueryString("query=string");
        method.setRequestBody(new NameValuePair[] { 
           new NameValuePair("param","eter"),
           new NameValuePair("para","meter") } );
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: POST</title>") >= 0);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<p>QueryString=\"query=string\"</p>") >= 0);
        assertTrue(method.getResponseBodyAsString(),method.getResponseBodyAsString().indexOf("name=\"param\";value=\"eter\"") >= 0);
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"para\";value=\"meter\"") >= 0);
    }
}

