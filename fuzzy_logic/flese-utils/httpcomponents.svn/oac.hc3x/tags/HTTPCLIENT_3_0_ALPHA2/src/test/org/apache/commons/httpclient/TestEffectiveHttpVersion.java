/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestEffectiveHttpVersion.java,v 1.2 2004/09/14 15:50:40 olegk Exp $
 * $Revision: 1.2 $
 * $Date: 2004-09-14 17:50:41 +0200 (Tue, 14 Sep 2004) $
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
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.commons.httpclient.server.HttpService;
import org.apache.commons.httpclient.server.SimpleRequest;
import org.apache.commons.httpclient.server.SimpleResponse;

/**
 * HTTP protocol versioning tests.
 *
 * @author Oleg Kalnichevski
 * 
 * @version $Revision: 1.2 $
 */
public class TestEffectiveHttpVersion extends HttpClientTestBase {

    // ------------------------------------------------------------ Constructor
    public TestEffectiveHttpVersion(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestEffectiveHttpVersion.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestEffectiveHttpVersion.class);
    }

    private class EchoService implements HttpService {

        public EchoService() {
            super();
        }

        public boolean process(final SimpleRequest request, final SimpleResponse response)
            throws IOException
        {
            HttpVersion httpversion = request.getRequestLine().getHttpVersion();
            response.setStatusLine(httpversion, HttpStatus.SC_OK);
            response.setBodyString(request.getBodyString());
            return true;
        }
    }

    public void testClientLevelHttpVersion() throws IOException {
        this.server.setHttpService(new EchoService());

        HttpVersion testver = new HttpVersion(1, 10);

        this.client.getParams().setVersion(testver);
        GetMethod httpget = new GetMethod("/test/");
        try {
            this.client.executeMethod(httpget);
        } finally {
            httpget.releaseConnection();
        }
        assertEquals(testver, httpget.getEffectiveVersion());
    }

    public void testMethodLevelHttpVersion() throws IOException {
        this.server.setHttpService(new EchoService());

        HttpVersion globalver = new HttpVersion(1, 10);
        HttpVersion testver1 = new HttpVersion(1, 11);
        HttpVersion testver2 = new HttpVersion(1, 12);

        this.client.getParams().setVersion(globalver);
        
        GetMethod httpget1 = new GetMethod("/test/");
        httpget1.getParams().setVersion(testver1);
        try {
            this.client.executeMethod(httpget1);
        } finally {
            httpget1.releaseConnection();
        }
        assertEquals(testver1, httpget1.getEffectiveVersion());

        GetMethod httpget2 = new GetMethod("/test/");
        httpget2.getParams().setVersion(testver2);
        try {
            this.client.executeMethod(httpget2);
        } finally {
            httpget2.releaseConnection();
        }
        assertEquals(testver2, httpget2.getEffectiveVersion());

        GetMethod httpget3 = new GetMethod("/test/");
        try {
            this.client.executeMethod(httpget3);
        } finally {
            httpget3.releaseConnection();
        }
        assertEquals(globalver, httpget3.getEffectiveVersion());
    }

    public void testHostLevelHttpVersion() throws IOException {
        this.server.setHttpService(new EchoService());

        HttpVersion testver = new HttpVersion(1, 11);
        HttpVersion hostver = new HttpVersion(1, 12);

        this.client.getParams().setVersion(testver);
        
        GetMethod httpget1 = new GetMethod("/test/");
        httpget1.getParams().setVersion(testver);
        
        HostConfiguration hostconf = new HostConfiguration();
        hostconf.setHost(this.server.getLocalAddress(), this.server.getLocalPort(), "http"); 
        try {
            this.client.executeMethod(hostconf, httpget1);
        } finally {
            httpget1.releaseConnection();
        }
        assertEquals(testver, httpget1.getEffectiveVersion());

        GetMethod httpget2 = new GetMethod("/test/");
        hostconf.setHost(this.server.getLocalAddress(), this.server.getLocalPort(), "http");
        hostconf.getParams().setParameter(HttpMethodParams.PROTOCOL_VERSION, hostver); 
        try {
            this.client.executeMethod(hostconf, httpget2);
        } finally {
            httpget2.releaseConnection();
        }
        assertEquals(hostver, httpget2.getEffectiveVersion());
    }
}
