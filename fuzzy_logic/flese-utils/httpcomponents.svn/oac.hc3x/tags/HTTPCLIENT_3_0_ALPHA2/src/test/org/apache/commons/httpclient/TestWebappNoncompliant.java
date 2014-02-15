/*
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
import org.apache.commons.httpclient.params.HttpMethodParams;

/**
 * Tests cases intended to test if entity enclosing methods
 * can deal with non-compliant HTTP servers or proxies
 * 
 * @author Oleg Kalnichevski
 * @author Jeff Dever
 */

public class TestWebappNoncompliant extends TestWebappBase
{
    public TestWebappNoncompliant(String s)
    {
        super(s);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestWebappNoncompliant.class);
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestWebappNoncompliant.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    /**
     * Tests if client is able able to recover gracefully when 
     * HTTP server or proxy fails to send 100 status code when
     * expected. The client should resume sending the request body 
     * after a defined timeout without having received "continue"
     * code.
     */
    public void testNoncompliantPostMethodString()
    {
        HttpClient client = createHttpClient();
        NoncompliantPostMethod method = new NoncompliantPostMethod("/" + getWebappContext() + "/body");
        method.getParams().setBooleanParameter(HttpMethodParams.USE_EXPECT_CONTINUE, true);
        method.setRequestEntity(new StringRequestEntity("This is data to be sent in the body of an HTTP POST."));
        try {
            client.executeMethod(method);
        } catch (Exception e) {
            e.printStackTrace();
            fail("Unexpected exception: " + e.toString());
        }
        assertEquals(200,method.getStatusCode());
    }

    /**
     */
    public void testNoncompliantStatusLine()
    {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod("/" + getWebappContext() + "/statusline");
        method.setRequestHeader("Set-StatusCode", 444+"");
        method.setRequestHeader("Set-StatusMessage", "This status message contains\n"
                + " a newline and a\r"
                + " carrage return but that should be OK.");
        try {
            client.executeMethod(method);
        } catch (Exception e) {
            e.printStackTrace();
            fail("Unexpected exception: " + e.toString());
        }
        assertEquals(444, method.getStatusCode());
    }


    /** 
     * Test if a response to HEAD method from non-compliant server
     * that contains an unexpected body content can be correctly redirected 
     */ 

    public void testNoncompliantHeadWithResponseBody() 
      throws Exception {
          HttpClient client = createHttpClient();
          HeadMethod method = new NoncompliantHeadMethod("/" + getWebappContext() + "/redirect");
          method.getParams().setIntParameter(HttpMethodParams.HEAD_BODY_CHECK_TIMEOUT, 50);
          client.executeMethod(method);
          assertEquals(200,method.getStatusCode());
          method.releaseConnection();
    }

    /** 
     * Test if a response to HEAD method from non-compliant server
     * causes an HttpException to be thrown 
     */ 

    public void testNoncompliantHeadStrictMode() 
      throws Exception {
          HttpClient client = createHttpClient();
          client.getParams().setBooleanParameter(HttpMethodParams.REJECT_HEAD_BODY, true);
          HeadMethod method = new NoncompliantHeadMethod("/" + getWebappContext() + "/body");
          method.getParams().setIntParameter(HttpMethodParams.HEAD_BODY_CHECK_TIMEOUT, 50);
          try {
              client.executeMethod(method);
              fail("HttpException should have been thrown"); 
          } catch(HttpException e) {
              // Expected
          }
          method.releaseConnection();
    }

}
