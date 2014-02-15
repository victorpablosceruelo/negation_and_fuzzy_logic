/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestWebappPostMethod.java,v 1.2 2003/02/03 21:21:19 olegk Exp $
 * $Revision: 1.2 $
 * $Date: 2003-02-03 22:21:19 +0100 (Mon, 03 Feb 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2003 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
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
import org.apache.commons.httpclient.methods.*;
import java.io.*;

/**
 * Webapp tests specific to the PostMethod.
 *
 * @author <a href="jsdever@apache.org">Jeff Dever</a>
 * @version $Id: TestWebappPostMethod.java 134080 2003-02-03 21:21:19Z olegk $
 */
public class TestWebappPostMethod extends TestWebappBase {

    HttpClient httpClient; 
    final static String paramsUrl = "http://" + host + ":" + port
        + "/" + context + "/params";
    final static String bodyUrl = "http://" + host + ":" + port
        + "/" + context + "/body";

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
        httpClient = new HttpClient();
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
        PostMethod method = new PostMethod(paramsUrl);
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
        PostMethod method = new PostMethod(paramsUrl);
        String stringBody = "pname1=pvalue1&pname2=pvalue2";

        method.setRequestBody(stringBody);
        method.setRequestHeader("Content-Type", PostMethod.FORM_URL_ENCODED_CONTENT_TYPE);
        
        verifyParams(method);
    }

    /**
     * Test that the body can be set as a String to the body servlet.
     */
    public void testStringBodyToBodyServlet() throws Exception {
        PostMethod method = new PostMethod(bodyUrl);
        String stringBody = "pname1=pvalue1&pname2=pvalue2";

        method.setRequestBody(stringBody);
        
        verifyBody(method);
    }

    /**
     * Test that the body can be set as a stream to the param servlet.
     */
    public void testStreamBodyToParamServlet() throws Exception {
        PostMethod method = new PostMethod(paramsUrl);
        InputStream streamBody = 
            new ByteArrayInputStream("pname1=pvalue1&pname2=pvalue2".getBytes());

        method.setRequestBody(streamBody);
        method.setRequestHeader("Content-Type", PostMethod.FORM_URL_ENCODED_CONTENT_TYPE);
        
        verifyParams(method);
    }

    /**
     * Test that the body can be set as a stream to the body servlet.
     */
    public void testStreamBodyToBodyServlet() throws Exception {
        PostMethod method = new PostMethod(bodyUrl);
        
        InputStream streamBody = 
            new ByteArrayInputStream("pname1=pvalue1&pname2=pvalue2".getBytes());
        method.setRequestBody(streamBody);
        
        verifyBody(method);
    }

    /**
     * Test that parameters can be added.
     */
    public void testAddParametersToParamServlet() throws Exception {
        PostMethod method = new PostMethod(paramsUrl);

        method.addParameter(new NameValuePair("pname1","pvalue1"));
        method.addParameter(new NameValuePair("pname2","pvalue2"));

        verifyParams(method);
    }

    /**
     * Test that parameters can be added and removed.
     */
    public void testAddRemoveParametersToParamServlet() throws Exception {
        PostMethod method = new PostMethod(paramsUrl);

        method.addParameter(new NameValuePair("pname0","pvalue0"));
        method.addParameter(new NameValuePair("pname1","pvalue1"));
        method.addParameter(new NameValuePair("pname2","pvalue2"));
        method.addParameter(new NameValuePair("pname3","pvalue3"));
        method.removeParameter("pname0");
        method.removeParameter("pname4");

        verifyParams(method);
    }

}

