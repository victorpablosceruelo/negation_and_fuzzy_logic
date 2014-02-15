/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestVirtualHost.java,v 1.2 2004/10/31 14:42:59 olegk Exp $
 * $Revision: 155418 $
 * $Date: 2005-02-26 14:01:52 +0100 (Sat, 26 Feb 2005) $
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
import org.apache.commons.httpclient.server.HttpService;
import org.apache.commons.httpclient.server.SimpleRequest;
import org.apache.commons.httpclient.server.SimpleResponse;

/**
 * HTTP protocol versioning tests.
 *
 * @author Oleg Kalnichevski
 * 
 * @version $Revision: 155418 $
 */
public class TestVirtualHost extends HttpClientTestBase {

    // ------------------------------------------------------------ Constructor
    public TestVirtualHost(final String testName) throws IOException {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestVirtualHost.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestVirtualHost.class);
    }

    private class VirtualService implements HttpService {

        public VirtualService() {
            super();
        }

        public boolean process(final SimpleRequest request, final SimpleResponse response)
            throws IOException
        {
            HttpVersion httpversion = request.getRequestLine().getHttpVersion();
            Header hostheader = request.getFirstHeader("Host");
            if (hostheader == null) {
                response.setStatusLine(httpversion, HttpStatus.SC_BAD_REQUEST);
                response.setBodyString("Host header missing");
            } else {
                response.setStatusLine(httpversion, HttpStatus.SC_OK);
                response.setBodyString(hostheader.getValue());
            }
            return true;
        }
    }

    public void testVirtualHostHeader() throws IOException {
        this.server.setHttpService(new VirtualService());

        GetMethod httpget = new GetMethod("/test/");
        
        HostConfiguration hostconf = new HostConfiguration();
        hostconf.setHost(this.server.getLocalAddress(), this.server.getLocalPort(), "http");
        hostconf.getParams().setVirtualHost("somehost");
        try {
            this.client.executeMethod(hostconf, httpget);
            String hostheader = "somehost:" + this.server.getLocalPort();
            assertEquals(hostheader, httpget.getResponseBodyAsString());
        } finally {
            httpget.releaseConnection();
        }
    }

    public void testNoVirtualHostHeader() throws IOException {
        this.server.setHttpService(new VirtualService());

        GetMethod httpget = new GetMethod("/test/");
        
        HostConfiguration hostconf = new HostConfiguration();
        hostconf.setHost(this.server.getLocalAddress(), this.server.getLocalPort(), "http");
        hostconf.getParams().setVirtualHost(null);
        try {
            this.client.executeMethod(hostconf, httpget);
            String hostheader = this.server.getLocalAddress() + ":" + this.server.getLocalPort();
            assertEquals(hostheader, httpget.getResponseBodyAsString());
        } finally {
            httpget.releaseConnection();
        }
    }
}
