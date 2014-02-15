/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestRedirects.java,v 1.2 2004/04/12 11:16:25 olegk Exp $
 * $Revision: 1.2 $
 * $Date: 2004-04-12 13:16:25 +0200 (Mon, 12 Apr 2004) $
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

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.params.HttpClientParams;
import org.apache.commons.httpclient.server.HttpService;
import org.apache.commons.httpclient.server.RequestLine;
import org.apache.commons.httpclient.server.SimpleRequest;
import org.apache.commons.httpclient.server.SimpleResponse;

/**
 * Basic authentication test cases.
 *
 * @author Oleg Kalnichevski
 * 
 * @version $Id: TestRedirects.java 134558 2004-04-12 11:16:25Z olegk $
 */
public class TestRedirects extends HttpClientTestBase {

    // ------------------------------------------------------------ Constructor
    public TestRedirects(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestRedirects.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestRedirects.class);
    }

    private class RedirectService implements HttpService {

        public RedirectService() {
            super();
        }

        public boolean process(final SimpleRequest request, final SimpleResponse response)
            throws IOException
        {
            RequestLine reqline = request.getRequestLine();
            if (reqline.getUri().equals("/circular-location1/")) {
                response.setStatusLine("HTTP/1.1 302 Object moved");
                response.addHeader(new Header("Location", "/circular-location2/"));
            } else if (reqline.getUri().equals("/circular-location2/")) {
                response.setStatusLine("HTTP/1.1 302 Object moved");
                response.addHeader(new Header("Location", "/circular-location1/"));
            } else {
                response.setStatusLine("HTTP/1.1 404 Not Found");
            }
            return true;
        }
    }

    public void testMaxRedirectCheck() throws IOException {
        this.server.setHttpService(new RedirectService());
        GetMethod httpget = new GetMethod("/circular-location1/");
        try {
            this.client.getParams().setBooleanParameter(HttpClientParams.ALLOW_CIRCULAR_REDIRECTS, true);
            this.client.getParams().setIntParameter(HttpClientParams.MAX_REDIRECTS, 5);
            this.client.executeMethod(httpget);
            fail("RedirectException exception should have been thrown");
        }
        catch (RedirectException e) {
            // expected
        } finally {
            httpget.releaseConnection();
        }
    }

    public void testCircularRedirect() throws IOException {
        this.server.setHttpService(new RedirectService());
        GetMethod httpget = new GetMethod("/circular-location1/");
        try {
            this.client.getParams().setBooleanParameter(HttpClientParams.ALLOW_CIRCULAR_REDIRECTS, false);
            this.client.executeMethod(httpget);
            fail("RedirectException exception should have been thrown");
        }
        catch (RedirectException e) {
            // expected
        } finally {
            httpget.releaseConnection();
        }
    }

    private class RedirectService2 implements HttpService {
    
        private String host = null;
        private int port;

        public RedirectService2(final String host, int port) {
            super();
            this.host = host;
            this.port = port;
        }

        public boolean process(final SimpleRequest request, final SimpleResponse response)
            throws IOException
        {
            RequestLine reqline = request.getRequestLine();
            if (reqline.getUri().equals("/location1/")) {
                response.setStatusLine("HTTP/1.1 302 Object moved");
                response.addHeader(new Header("Location", "http://" + this.host + ":" + this.port + "/location2/"));
            } else if (reqline.getUri().equals("/location2/")) {
                response.setStatusLine("HTTP/1.1 200 OK");
                response.setBodyString("Successful redirect");
            } else {
                response.setStatusLine("HTTP/1.1 404 Not Found");
            }
            return true;
        }
    }

    public void testRedirectLocation() throws IOException {
        String host = this.server.getLocalAddress();
        int port = this.server.getLocalPort();
        this.server.setHttpService(new RedirectService2(host, port));
        GetMethod httpget = new GetMethod("/location1/");
        try {
            this.client.executeMethod(httpget);
            assertEquals(host, httpget.getURI().getHost());
            assertEquals(port, httpget.getURI().getPort());
            assertEquals(new URI("http://" + host + ":" + port + "/location2/", false), httpget.getURI());
        } finally {
            httpget.releaseConnection();
        }
    }

}
