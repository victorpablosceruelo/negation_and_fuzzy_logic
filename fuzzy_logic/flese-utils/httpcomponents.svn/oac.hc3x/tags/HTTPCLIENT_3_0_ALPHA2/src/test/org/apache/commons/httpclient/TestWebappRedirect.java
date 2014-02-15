/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestWebappRedirect.java,v 1.23 2004/05/12 20:43:54 olegk Exp $
 * $Revision: 1.23 $
 * $Date: 2004-05-12 22:43:54 +0200 (Wed, 12 May 2004) $
 *
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

import java.io.ByteArrayInputStream;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.ByteArrayRequestEntity;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.PutMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;
import org.apache.commons.httpclient.util.EncodingUtil;
import org.apache.commons.httpclient.util.URIUtil;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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
 * @version $Id: TestWebappRedirect.java 134607 2004-05-12 20:43:54Z olegk $
 */
public class TestWebappRedirect extends TestWebappBase {
    
    private static final Log log = LogFactory.getLog(TestWebappRedirect.class);

    private final String redirectUrl = "/" + getWebappContext() + "/redirect";

    private final String paramsUrl =  "/" + getWebappContext() + "/params";

    private final String bodyUrl =  "/" + getWebappContext() + "/body";

    public TestWebappRedirect(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestWebappRedirect.class);
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestWebappRedirect.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    public void absoluteRedirectHelper(int code) throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod(redirectUrl);
        method.setQueryString("to=" + paramsUrl + "&code=" + code);
        client.executeMethod(method);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
    }


    // ------------------------------------------------------------------ Tests

    public void testAbsoluteRedirectCode301() throws Exception {
        absoluteRedirectHelper(301);
    }

    public void testAbsoluteRedirectCode302() throws Exception {
        absoluteRedirectHelper(302);
    }

    public void testAbsoluteRedirectCode303() throws Exception {
        absoluteRedirectHelper(303);
    }

    public void testAbsoluteRedirectCode307() throws Exception {
        absoluteRedirectHelper(307);
    }

    public void testRelativeRedirect() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod(redirectUrl);
        method.setQueryString("to=params");
        client.executeMethod(method);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
    }


    public void testRedirectWithQueryString() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod(redirectUrl);
        method.setQueryString(new NameValuePair[] {
            new NameValuePair("to", paramsUrl + "?foo=bar&bar=foo")
            }
        );
        client.executeMethod(method);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertTrue(method.getResponseBodyAsString().indexOf("<p>QueryString=\"foo=bar&bar=foo\"</p>") >= 0);
    }

    public void testRecursiveRedirect() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod(redirectUrl);

        String qs = paramsUrl + "?foo=bar&bar=foo";
        for(int i=0;i<2;i++) {
            qs = redirectUrl + "?to=" + URIUtil.encodeWithinQuery(qs);
        }
        method.setQueryString("to=" + URIUtil.encodeWithinQuery(qs));
        client.executeMethod(method);
        assertEquals(200,method.getStatusCode());
        assertTrue(method.getResponseBodyAsString().indexOf("<title>Param Servlet: GET</title>") >= 0);
        assertTrue(method.getResponseBodyAsString().indexOf("<p>QueryString=\"foo=bar&bar=foo\"</p>") >= 0);
    }

    public void testDetectRedirectLoop() throws Exception {
        HttpClient client = createHttpClient();
        GetMethod method = new GetMethod(redirectUrl);
        method.setQueryString("loop=true");
        try {
            client.executeMethod(method);
            fail("Expected HTTPException");
        } catch (ProtocolException t) {
            // expected
        }
        assertEquals(302,method.getStatusCode());
        assertTrue(null != method.getResponseHeader("location"));
        assertTrue(null != (method.getResponseHeader("location")).getValue());
        assertEquals(client.getHostConfiguration().getHostURL() + "/" + getWebappContext() + "/redirect?loop=true",(method.getResponseHeader("location")).getValue());
        log.info("Previous redirect loop warining is okay");
    }

    public void testPostRedirect() throws Exception {
        String bodyStr = "Hello World";
        HttpClient client = createHttpClient();
        PostMethod method = new PostMethod(redirectUrl);
        method.setQueryString("to=" + URIUtil.encodeWithinQuery(
                    client.getHostConfiguration().getHostURL() + "/" 
                    + getWebappContext() + "/params?foo=bar&bar=foo"));
        byte[] body = EncodingUtil.getBytes(bodyStr, "ISO-8859-1");
        method.setRequestEntity(new ByteArrayRequestEntity(body));
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        //unbuffered request can not be redirected
        assertEquals(HttpStatus.SC_MOVED_TEMPORARILY,method.getStatusCode());

        method = new PostMethod(redirectUrl);
        method.setQueryString("to=" + URIUtil.encodeWithinQuery(paramsUrl + "?foo=bar&bar=foo"));
        method.setRequestEntity(new InputStreamRequestEntity(new ByteArrayInputStream(body)));
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        //buffered request is okay to redirect
        assertEquals(HttpStatus.SC_MOVED_TEMPORARILY,method.getStatusCode());
    }

    public void testPutRedirect() throws Exception {
        HttpClient client = createHttpClient();
        PutMethod method = new PutMethod(redirectUrl);
        method.setQueryString("to=" + URIUtil.encodeWithinQuery(bodyUrl + "?foo=bar&bar=foo"));
        method.setRequestEntity(new StringRequestEntity("This is data to be sent in the body of an HTTP PUT."));
        client.executeMethod(method);
        assertEquals(HttpStatus.SC_MOVED_TEMPORARILY,method.getStatusCode());
    }
}

