/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestWebappPostMethod.java,v 1.7 2004/05/12 20:43:54 olegk Exp $
 * $Revision: 1.7 $
 * $Date: 2004-05-12 22:43:54 +0200 (Wed, 12 May 2004) $
 *
 * ====================================================================
 *
 *  Copyright 2003-2004 The Apache Software Foundation
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
import java.io.*;

/**
 * Webapp tests specific to the PostMethod.
 *
 * @author <a href="jsdever@apache.org">Jeff Dever</a>
 * @version $Id: TestWebappPostMethod.java 134607 2004-05-12 20:43:54Z olegk $
 */
public class TestWebappPostMethod extends TestWebappBase {

    HttpClient httpClient; 
    final String paramsPath = "/" + getWebappContext() + "/params";
    final String bodyPath = "/" + getWebappContext() + "/body";

    public TestWebappPostMethod(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestWebappPostMethod.class);
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestWebappPostMethod.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    public void setUp() {
        httpClient = createHttpClient();
    }

    /**
     * Helper method for performing a routine test.
     */
    private void verifyBody(PostMethod method) throws Exception {
        httpClient.executeMethod(method);

        assertEquals(200,method.getStatusCode());
        String body = method.getResponseBodyAsString();
        //System.out.println(body);
        assertTrue(body.indexOf("Body Servlet: POST") >= 0);
        assertTrue(body.indexOf("pname1=pvalue1&pname2=pvalue2") >= 0);
    }


    /**
     * Helper method for performing a routine test.
     */
    private void verifyParams(PostMethod method) throws Exception {
        httpClient.executeMethod(method);

        assertEquals(200,method.getStatusCode());
        String body = method.getResponseBodyAsString();
        //System.out.println(body);
        assertTrue(body.indexOf("Param Servlet: POST") >= 0);
        assertTrue(body.indexOf("QueryString=null") >= 0);
        assertTrue(body.indexOf("name=\"pname1\";value=\"pvalue1\"") >= 0);
        assertTrue(body.indexOf("name=\"pname2\";value=\"pvalue2\"") >= 0);
    }


    // ------------------------------------------------------------------ Tests
    
    /**
     * Test that the body can be set as a array or parameters the param servlet.
     */
    public void testParametersBodyToParamServlet() throws Exception {
        PostMethod method = new PostMethod(paramsPath);
        NameValuePair[] parametersBody =  new NameValuePair[] { 
            new NameValuePair("pname1","pvalue1"),
            new NameValuePair("pname2","pvalue2") 
        };

        method.setRequestBody(parametersBody);

        verifyParams(method);
    }

    /**
     * Test that the body can be set as a String to the param servlet.
     */
    public void testStringBodyToParamServlet() throws Exception {
        PostMethod method = new PostMethod(paramsPath);
        String stringBody = "pname1=pvalue1&pname2=pvalue2";

        method.setRequestEntity(
            new StringRequestEntity(stringBody, PostMethod.FORM_URL_ENCODED_CONTENT_TYPE, null));
        
        verifyParams(method);
    }

    /**
     * Test that the body can be set as a String to the body servlet.
     */
    public void testStringBodyToBodyServlet() throws Exception {
        PostMethod method = new PostMethod(bodyPath);
        String stringBody = "pname1=pvalue1&pname2=pvalue2";

        method.setRequestEntity(new StringRequestEntity(stringBody));
        
        verifyBody(method);
    }

    /**
     * Test that parameters can be added.
     */
    public void testAddParametersToParamServlet() throws Exception {
        PostMethod method = new PostMethod(paramsPath);

        method.addParameter(new NameValuePair("pname1","pvalue1"));
        method.addParameter(new NameValuePair("pname2","pvalue2"));

        verifyParams(method);
    }

    /**
     * Test that parameters can be added and removed.
     */
    public void testAddRemoveParametersToParamServlet() throws Exception {
        PostMethod method = new PostMethod(paramsPath);

        method.addParameter(new NameValuePair("pname0","pvalue0"));
        method.addParameter(new NameValuePair("pname1","pvalue1"));
        method.addParameter(new NameValuePair("pname2","pvalue2"));
        method.addParameter(new NameValuePair("pname3","pvalue3"));
        method.removeParameter("pname0");
        method.removeParameter("pname4");

        verifyParams(method);
    }

    /**
     * Test the return value of the PostMethod#removeParameter.
     */
    public void testRemoveParameterReturnValue() throws Exception {
        PostMethod method = new PostMethod(paramsPath);

        method.addParameter("param", "whatever");
        assertTrue("Return value of the method is expected to be true", method.removeParameter("param"));
        assertFalse("Return value of the method is expected to be false", method.removeParameter("param"));
    }

    private String getRequestAsString(RequestEntity entity) throws Exception {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        entity.writeRequest(bos);
        return new String(bos.toByteArray(), "UTF-8");
    }
    
    /**
     * Test if setParameter overwrites existing parameter values.
     */
    public void testAddParameterFollowedBySetParameter() throws Exception {
        PostMethod method = new PostMethod(paramsPath);

        method.addParameter("param", "a");
        method.addParameter("param", "b");
        method.addParameter("param", "c");
        assertEquals("param=a&param=b&param=c", getRequestAsString(method.getRequestEntity()));
        method.setParameter("param", "a");
        assertEquals("param=a", getRequestAsString(method.getRequestEntity()));
    }

}

