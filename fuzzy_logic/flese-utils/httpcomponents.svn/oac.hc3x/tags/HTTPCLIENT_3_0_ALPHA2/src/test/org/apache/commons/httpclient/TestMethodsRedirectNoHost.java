/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestMethodsRedirectNoHost.java,v 1.11 2004/04/24 23:51:57 olegk Exp $
 * $Revision: 1.11 $
 * $Date: 2004-04-25 01:51:57 +0200 (Sun, 25 Apr 2004) $
 * ====================================================================
 *
 *  Copyright 2002-2004 The Apache Software Foundation
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

import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.params.HttpClientParams;

/**
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @version $Revision: 1.11 $
 */
public class TestMethodsRedirectNoHost extends TestNoHostBase {

    
    // ------------------------------------------------------------ Constructor

    public TestMethodsRedirectNoHost(String testName) {
        super(testName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestMethodsRedirectNoHost.class);
    }

    private void addRedirectResponse(String location) {
        String headers = "HTTP/1.1 302 Redirect\r\n"
                       +"Date: Wed, 28 Mar 2002 05:05:04 GMT\r\n"
                       +"Location: " + location + "\r\n"
                       +"Connection: close\r\n";
        conn.addResponse(headers, "");
    }

    private void addOkResponse() {
        String headers = "HTTP/1.1 200 OK\r\n"
                       +"Date: Wed, 28 Mar 2001 05:05:04 GMT\r\n"
                       +"Connection: close\r\n";
        conn.addResponse(headers, "");
    }


    // ----------------------------------------------------------------- Tests

    public void testRedirect() throws Exception {
        addRedirectResponse("http://localhost/newfile");
        addOkResponse();
        conn.open();

        HttpMethod method = new SimpleHttpMethod("http://localhost/oldfile");
        method.setFollowRedirects(true);
        client.executeMethod(method);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(200, method.getStatusCode());
        assertEquals("/newfile", method.getPath());
        
    }


    public void testRedirectIgnoreCase() throws Exception {
        addRedirectResponse("HtTP://localhost/newfile");
        addOkResponse();
        conn.open();

        HttpMethod method = new SimpleHttpMethod("/oldfile");
        method.setFollowRedirects(true);
        client.executeMethod(method);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(200, method.getStatusCode());
        assertEquals("/newfile", method.getPath());
        
    }


    public void testPostRedirect() throws Exception {
        addRedirectResponse("http://localhost/newfile");
        addOkResponse();
        conn.open();

        PostMethod method = new PostMethod("/oldfile");
        method.setRequestBody(new NameValuePair[] { new NameValuePair("name", "value") } );
        client.executeMethod(method);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(302, method.getStatusCode());
        assertEquals("/oldfile", method.getPath());
        
    }


    public void testNoRedirect() throws Exception {

        addRedirectResponse("http://localhost/newfile");
        addOkResponse();
        conn.open();

        HttpMethod method = new SimpleHttpMethod("/oldfile");
        method.setFollowRedirects(false);
        method.execute(new HttpState(), conn);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(302, method.getStatusCode());
        assertEquals("/oldfile", method.getPath());
        
    }
 

    public void testRedirectBadLocation() throws Exception {
        addRedirectResponse("newfile");
        addOkResponse();
        conn.open();

        HttpMethod method = new SimpleHttpMethod("/oldfile");
        method.setFollowRedirects(true);
        client.getParams().setBooleanParameter(
            HttpClientParams.REJECT_RELATIVE_REDIRECT, false);
        client.executeMethod(method);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(200, method.getStatusCode());
        assertEquals("/newfile", method.getPath());
    }

   
    public void testRedirectBadLocationStrict() throws Exception {
        addRedirectResponse("newfile");
        addOkResponse();
        conn.open();

        HttpMethod method = new SimpleHttpMethod("/oldfile");
        method.setFollowRedirects(true);
        client.getParams().setBooleanParameter(
            HttpClientParams.REJECT_RELATIVE_REDIRECT, true);
        method.execute(new HttpState(), conn);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(302, method.getStatusCode());
        assertEquals("/oldfile", method.getPath());
    }   

    public void testRedirectBogusLocationStrict() throws Exception {
        addRedirectResponse("xxx://bogus");
        addOkResponse();
        conn.open();

        HttpMethod method = new SimpleHttpMethod("/oldfile");
        method.setFollowRedirects(true);
        client.getParams().setBooleanParameter(
            HttpClientParams.REJECT_RELATIVE_REDIRECT, false);
        method.execute(new HttpState(), conn);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(302, method.getStatusCode());
        assertEquals("/oldfile", method.getPath());
    }

    public void testRedirectDifferentHost() throws Exception {
        
        addRedirectResponse("http://newhost/newfile");
        addOkResponse();

        HttpMethod method = new SimpleHttpMethod("/oldfile");
        method.setFollowRedirects(true);
        client.executeMethod(method);
        assertEquals(200, method.getStatusCode());
        assertEquals("/newfile", method.getPath());
        assertEquals("newhost", method.getRequestHeader("host").getValue());
    }

    public void testRedirectDifferentPort() throws Exception {
        conn = new SimpleHttpConnection("oldhost", 80);
        addRedirectResponse("http://oldhost:8080/newfile");
        addOkResponse();
        conn.open();

        HttpMethod method = new SimpleHttpMethod("/oldfile");
        method.setFollowRedirects(true);
        method.execute(new HttpState(), conn);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(302, method.getStatusCode());
        assertEquals("/oldfile", method.getPath());
    }


    public void testRedirectDifferentProtocol() throws Exception {
        conn = new SimpleHttpConnection("oldhost", 80);
        addRedirectResponse("https://oldhost:80/newfile");
        addOkResponse();
        conn.open();

        HttpMethod method = new SimpleHttpMethod("/oldfile");
        method.setFollowRedirects(true);
        method.execute(new HttpState(), conn);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(302, method.getStatusCode());
        assertEquals("/oldfile", method.getPath());
    }


    public void testRedirectWithCookie() throws Exception {
        addRedirectResponse("http://localhost/newfile");
        addOkResponse();
        conn.open();

        client.getState().addCookie(new Cookie("localhost", "name", "value", "/", -1, false)); 

        HttpMethod method = new SimpleHttpMethod("/oldfile");
        method.setFollowRedirects(true);
        client.executeMethod(method);
        Header locationHeader = method.getResponseHeader("Location");
        assertEquals(200, method.getStatusCode());

        Header[] headers = method.getRequestHeaders();
        int cookiecount = 0;
        for (int i = 0; i < headers.length; i++) {
            if ("cookie".equalsIgnoreCase(headers[i].getName())) {
                ++cookiecount;
            }
        }
        assertEquals("There can only be one (cookie)", 1, cookiecount);            
    }

}
