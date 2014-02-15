/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestRequestHeaders.java,v 1.6 2004/02/22 18:08:49 olegk Exp $
 * $Revision: 1.6 $
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Tests for reading response headers.
 *
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @version $Id: TestRequestHeaders.java 134530 2004-02-22 18:08:52Z olegk $
 */
public class TestRequestHeaders extends TestCase {

       HttpState state = null;
       SimpleHttpMethod method = null;
       HttpConnection conn = null;

    // ------------------------------------------------------------ Constructor
    public TestRequestHeaders(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = {TestRequestHeaders.class.getName()};
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods
    public static Test suite() {
        return new TestSuite(TestRequestHeaders.class);
    }

    public void setUp() {
       state = new HttpState();
       method = new SimpleHttpMethod("/some/absolute/path/");
       //assign conn in test case
    }

    public void tearDown() {
       state = null;
       method = null;
       conn = null;
    }

    public void testNullHeader() throws Exception {
       conn = new SimpleHttpConnection("some.host.name", 80, "http");
       assertEquals(null, method.getRequestHeader(null));
       assertEquals(null, method.getRequestHeader("bogus"));
    }

    public void testHostHeaderPortHTTP80() throws Exception {
       conn = new SimpleHttpConnection("some.host.name", 80, "http");
       method.addRequestHeaders(state, conn);
       assertEquals("Host: some.host.name", method.getRequestHeader("Host").toString().trim());
    }

    public void testHostHeaderPortHTTP81() throws Exception {
       conn = new SimpleHttpConnection("some.host.name", 81, "http");
       method.addRequestHeaders(state, conn);
       assertEquals("Host: some.host.name:81", method.getRequestHeader("Host").toString().trim());
    }

    public void testHostHeaderPortHTTPS443() throws Exception {
       conn = new SimpleHttpConnection("some.host.name", 443, "https");
       method.addRequestHeaders(state, conn);
       assertEquals("Host: some.host.name", method.getRequestHeader("Host").toString().trim());
    }

    public void testHostHeaderPortHTTPS444() throws Exception {
       conn = new SimpleHttpConnection("some.host.name", 444, "https");
       method.addRequestHeaders(state, conn);
       assertEquals("Host: some.host.name:444", method.getRequestHeader("Host").toString().trim());
    }

    public void testHeadersPreserveCaseKeyIgnoresCase() throws Exception {
        method.addRequestHeader(new Header("NAME", "VALUE"));
        Header upHeader =  method.getRequestHeader("NAME");
        Header loHeader =  method.getRequestHeader("name");
        Header mixHeader =  method.getRequestHeader("nAmE");
        assertEquals("NAME", upHeader.getName());
        assertEquals("VALUE", upHeader.getValue());
        assertEquals("NAME", loHeader.getName());
        assertEquals("VALUE", loHeader.getValue());
        assertEquals("NAME", mixHeader.getName());
        assertEquals("VALUE", mixHeader.getValue());
    }
}
