/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-beta2/module-client/src/test/java/org/apache/http/impl/client/TestDefaultConnKeepAliveStrategy.java $
 * $Revision: 672367 $
 * $Date: 2008-06-27 21:49:20 +0200 (Fri, 27 Jun 2008) $
 * ====================================================================
 *
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
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
 */

package org.apache.http.impl.client;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.conn.ConnectionKeepAliveStrategy;
import org.apache.http.message.BasicHttpResponse;
import org.apache.http.message.BasicStatusLine;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;

/**
 *  Simple tests for {@link DefaultConnectionKeepAliveStrategy}.
 * 
 * @version $Revision: 672367 $
 */
public class TestDefaultConnKeepAliveStrategy extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestDefaultConnKeepAliveStrategy(final String testName) throws IOException {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestDefaultConnKeepAliveStrategy.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestDefaultConnKeepAliveStrategy.class);
    }

    public void testIllegalResponseArg() throws Exception {
        HttpContext context = new BasicHttpContext(null);
        ConnectionKeepAliveStrategy keepAliveStrat = new DefaultConnectionKeepAliveStrategy();
        try {
            keepAliveStrat.getKeepAliveDuration(null, context);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }

    public void testNoKeepAliveHeader() throws Exception {
        HttpContext context = new BasicHttpContext(null);
        HttpResponse response = new BasicHttpResponse(
                new BasicStatusLine(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK"));
        ConnectionKeepAliveStrategy keepAliveStrat = new DefaultConnectionKeepAliveStrategy();
        long d = keepAliveStrat.getKeepAliveDuration(response, context);
        assertEquals(-1, d);
    }

    public void testEmptyKeepAliveHeader() throws Exception {
        HttpContext context = new BasicHttpContext(null);
        HttpResponse response = new BasicHttpResponse(
                new BasicStatusLine(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK"));
        response.addHeader("Keep-Alive", "timeout, max=20");
        ConnectionKeepAliveStrategy keepAliveStrat = new DefaultConnectionKeepAliveStrategy();
        long d = keepAliveStrat.getKeepAliveDuration(response, context);
        assertEquals(-1, d);
    }

    public void testInvalidKeepAliveHeader() throws Exception {
        HttpContext context = new BasicHttpContext(null);
        HttpResponse response = new BasicHttpResponse(
                new BasicStatusLine(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK"));
        response.addHeader("Keep-Alive", "timeout=whatever, max=20");
        ConnectionKeepAliveStrategy keepAliveStrat = new DefaultConnectionKeepAliveStrategy();
        long d = keepAliveStrat.getKeepAliveDuration(response, context);
        assertEquals(-1, d);
    }

    public void testKeepAliveHeader() throws Exception {
        HttpContext context = new BasicHttpContext(null);
        HttpResponse response = new BasicHttpResponse(
                new BasicStatusLine(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK"));
        response.addHeader("Keep-Alive", "timeout=300, max=20");
        ConnectionKeepAliveStrategy keepAliveStrat = new DefaultConnectionKeepAliveStrategy();
        long d = keepAliveStrat.getKeepAliveDuration(response, context);
        assertEquals(300000, d);
    }

}
