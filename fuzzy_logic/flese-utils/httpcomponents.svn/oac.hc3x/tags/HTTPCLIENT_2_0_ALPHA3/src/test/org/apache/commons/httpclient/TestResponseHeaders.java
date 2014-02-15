/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestResponseHeaders.java,v 1.6 2003/01/23 22:48:27 jsdever Exp $
 * $Revision: 1.6 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Tomcat", and "Apache Software
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Tests for reading response headers.
 *
 * @author <a href="mailto:dims@apache.org">Davanum Srinivas</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @version $Id: TestResponseHeaders.java 134019 2003-01-23 22:48:49Z jsdever $
 */
public class TestResponseHeaders extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestResponseHeaders(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = {TestResponseHeaders.class.getName()};
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods
    public static Test suite() {
        return new TestSuite(TestResponseHeaders.class);
    }



    /**
     * Simple extension of HttpMethodBase.
     */
    private class SimpleHttpMethod extends HttpMethodBase {
        public SimpleHttpMethod() {
            super("");
        }
        public String getName() {
            return "simple";
        }
    }

    // ----------------------------------------------------------- Test Methods
    public void testHeaders() throws Exception {
        String body = "XXX\r\nYYY\r\nZZZ";
        String headers =
                "HTTP/1.1 200 OK\r\n" +
                "Connection: close\r\n" +
                "Content-Length: " + body.length() + "\r\n" +
                "Content-Type: text/xml; charset=utf-8\r\n" +
                "Date: Wed, 28 Mar 2001 05:05:04 GMT\r\n" +
                "Server: UserLand Frontier/7.0-WinNT\r\n";
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod();
        SimpleHttpConnection conn = new SimpleHttpConnection(headers, body);
        method.execute(state, conn);
        assertEquals("close", method.getResponseHeader("Connection").getValue());
        assertEquals(body.length(), Integer.parseInt(method.getResponseHeader("Content-Length").getValue()));
        assertEquals("text/xml; charset=utf-8", method.getResponseHeader("Content-Type").getValue());
        assertEquals("Wed, 28 Mar 2001 05:05:04 GMT", method.getResponseHeader("Date").getValue());
        assertEquals("UserLand Frontier/7.0-WinNT", method.getResponseHeader("Server").getValue());
    }

    /**
     * Tests that having a duplicate content length causes no problems.
     */    
    public void testDuplicateContentLength() throws Exception {
        
        String body = "XXX\r\nYYY\r\nZZZ";
        String headers =
                "HTTP/1.1 200 OK\r\n" +
                "Content-Length: " + body.length() + "\r\n" +
                "Content-Length: " + body.length() + "\r\n";
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod();
        SimpleHttpConnection conn = new SimpleHttpConnection(headers, body);
        method.execute(state, conn);
        assertNotNull( "Response body is null.", method.getResponseBodyAsStream() );
                
    }

    public void testNullHeaders() throws Exception {
        String body = "XXX\r\nYYY\r\nZZZ";
        String headers =
                "HTTP/1.1 200 OK\r\n" +
                "Content-Length: " + body.length() + "\r\n";
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod();
        SimpleHttpConnection conn = new SimpleHttpConnection(headers, body);
        method.execute(state, conn);
        assertEquals(null, method.getResponseHeader(null));
        assertEquals(null, method.getResponseHeader("bogus"));
    }
    
    public void testFoldedHeaders() throws Exception {
        String body = "XXX\r\nYYY\r\nZZZ";
        String headers =
                "HTTP/1.1 200 OK\r\n" +
                "Connection: close\r\n" +
                "Content-Length: " + body.length() + "\r\n" +
                "Content-Type: text/xml; charset=utf-8\r\n" +
                "\tboundary=XXXX\r\n" +
                "Date: Wed, 28 Mar 2001\r\n" + 
                " 05:05:04 GMT\r\n" +
                "Server: UserLand Frontier/7.0-WinNT\r\n";
        HttpState state = new HttpState();
        HttpMethod method = new SimpleHttpMethod();
        SimpleHttpConnection conn = new SimpleHttpConnection(headers, body);
        method.execute(state, conn);
        assertEquals("close", method.getResponseHeader("Connection").getValue());
        assertEquals(body.length(), Integer.parseInt(method.getResponseHeader("Content-Length").getValue()));
        assertEquals("text/xml; charset=utf-8 boundary=XXXX", method.getResponseHeader("Content-Type").getValue());
        assertEquals("Wed, 28 Mar 2001 05:05:04 GMT", method.getResponseHeader("Date").getValue());
        assertEquals("UserLand Frontier/7.0-WinNT", method.getResponseHeader("Server").getValue());
        assertTrue(method.getResponseHeader("Content-Type").toString().indexOf("boundary") != -1);
    }
}
