/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestResponseHeaders.java,v 1.14 2004/02/26 20:25:55 olegk Exp $
 * $Revision: 1.14 $
 * $Date: 2004-02-26 21:25:55 +0100 (Thu, 26 Feb 2004) $
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
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.GetMethod;

/**
 * Tests for reading response headers.
 *
 * @author <a href="mailto:dims@apache.org">Davanum Srinivas</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:adrian@intencha.com">Adrian Sutton</a>
 * @version $Id: TestResponseHeaders.java 134537 2004-02-26 20:25:55Z olegk $
 */
public class TestResponseHeaders extends TestNoHostBase {

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
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(headers, body);
        client.executeMethod(method);
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
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(headers, body);
        client.executeMethod(method);
        assertNotNull( "Response body is null.", method.getResponseBodyAsStream() );
                
    }

    public void testDuplicateProxyConnection() throws Exception {
        
        client.getHostConfiguration().setProxy("proxy", 1);

        String headers = 
            "HTTP/1.1 200 OK\r\n"
            + "proxy-connection: close\r\n"
            + "proxy-connection: close\r\n"
            + "Content-Length: 0\r\n"
            + "\r\n";

        conn.addResponse(headers, "");

        GetMethod method = new GetMethod("/");
        client.executeMethod(method);
        method.getResponseBodyAsString();
        
        assertFalse(conn.isOpen());
        
        headers = 
            "HTTP/1.0 200 OK\r\n"
            + "proxy-connection: keep-alive\r\n"
            + "proxy-connection: keep-alive\r\n"
            + "Content-Length: 2\r\n"
            + "\r\n";

        conn.addResponse(headers, "");
        method = new GetMethod("/");
        client.executeMethod(method);
        method.getResponseBodyAsString();
        
        assertTrue(conn.isOpen());        
    }

    public void testDuplicateConnection() throws Exception {
        
        String headers = 
            "HTTP/1.1 200 OK\r\n"
            + "Connection: close\r\n"
            + "Connection: close\r\n"
            + "Content-Length: 0\r\n"
            + "\r\n";

        GetMethod method = new GetMethod("/");
        conn.addResponse(headers, "");
        client.executeMethod(method);
        method.getResponseBodyAsString();
        
        assertFalse(conn.isOpen());

        headers = 
            "HTTP/1.0 200 OK\r\n"
            +"Connection: keep-alive\r\n"
            +"Connection: keep-alive\r\n"
            + "Content-Length: 2\r\n"
            +"\r\n";

        method = new GetMethod("/");
        conn.addResponse(headers, "");
        client.executeMethod(method);
        method.getResponseBodyAsString();
        
        assertTrue(conn.isOpen());
    }
    
    public void testNoContentLength() throws Exception {
        // test with connection header
        String headers = 
            "HTTP/1.1 200 OK\r\n"
            + "Connection: keep-alive\r\n"
            + "\r\n";

        GetMethod method = new GetMethod("/");
        conn.addResponse(headers, "12345");
        client.executeMethod(method);
        method.getResponseBodyAsString();
        
        assertFalse(conn.isOpen());
        
        // test without connection header
        headers = "HTTP/1.1 200 OK\r\n\r\n";

        // test with connection header
        method = new GetMethod("/");
        conn.addResponse(headers, "12345");
        client.executeMethod(method);
        method.getResponseBodyAsString();
        
        assertFalse(conn.isOpen());
    }

    public void testInvalidContentLength1() throws Exception {
        // test with connection header
        String headers = "HTTP/1.1 200 OK\r\n"
            + "Content-Length: 5\r\n"
            + "Content-Length: stuff\r\n"
            + "\r\n";

        // test with connection header
        conn.addResponse(headers, "12345");
        GetMethod method = new GetMethod("/");
        client.executeMethod(method);
        assertEquals(5, method.getResponseContentLength()); 
    }

    public void testInvalidContentLength2() throws Exception {
        // test with connection header
        String headers = "HTTP/1.1 200 OK\r\n"
            + "Content-Length: stuff\r\n"
            + "Content-Length: 5\r\n"
            + "\r\n";

        // test with connection header
        conn.addResponse(headers, "12345");
        GetMethod method = new GetMethod("/");
        client.executeMethod(method);
        assertEquals(5, method.getResponseContentLength()); 
    }

    public void testProxyNoContentLength() throws Exception {
        // test with proxy-connection header
        String headers =
            "HTTP/1.1 200 OK\r\n"
            + "proxy-connection: keep-alive\r\n"
            + "\r\n";

        conn.setProxyHost("proxy");
        conn.setProxyPort(1);
        GetMethod method = new GetMethod("/");
        conn.addResponse(headers, "12345");
        client.executeMethod(method);
        method.getResponseBodyAsString();
        
        assertFalse(conn.isOpen());

        // test without proxy-connection header
        headers = "HTTP/1.1 200 OK\r\n\r\n";

        conn.setProxyHost("proxy");
        conn.setProxyPort(1);
        method = new GetMethod("/");
        conn.addResponse(headers, "12345");
        client.executeMethod(method);
        method.getResponseBodyAsString();
        
        assertFalse(conn.isOpen());
    }

    public void testNullHeaders() throws Exception {
        String body = "XXX\r\nYYY\r\nZZZ";
        String headers =
                "HTTP/1.1 200 OK\r\n" +
                "Content-Length: " + body.length() + "\r\n";
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(headers, body);
        client.executeMethod(method);
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
        HttpMethod method = new SimpleHttpMethod();
        conn.addResponse(headers, body);
        client.executeMethod(method);
        assertEquals("close", method.getResponseHeader("Connection").getValue());
        assertEquals(body.length(), Integer.parseInt(method.getResponseHeader("Content-Length").getValue()));
        assertEquals("text/xml; charset=utf-8 boundary=XXXX", method.getResponseHeader("Content-Type").getValue());
        assertEquals("Wed, 28 Mar 2001 05:05:04 GMT", method.getResponseHeader("Date").getValue());
        assertEquals("UserLand Frontier/7.0-WinNT", method.getResponseHeader("Server").getValue());
        assertTrue(method.getResponseHeader("Content-Type").toString().indexOf("boundary") != -1);
    }


	public void testForceCloseConnection() throws Exception {
		String body = "stuff";
		String headers =
				"HTTP/1.1 200 OK\r\n" +
				"Content-Type: garbage\r\n" +
                "\r\n";
		SimpleHttpMethod method = new SimpleHttpMethod();
        conn.addResponse(headers, body);
        client.executeMethod(method);
		assertTrue("Connection should be closed", method.shouldCloseConnection(conn));
		assertTrue("Connection should be force-closed", method.isConnectionCloseForced());
	}
    
	public void testForceCloseConnection2() throws Exception {
		String body = "stuff";
		String headers =
				"HTTP/1.1 200 OK\r\n" +
				"Content-Type: garbage\r\n" +
				"Connection: close\r\n" +
                "\r\n";
		SimpleHttpMethod method = new SimpleHttpMethod();
        conn.addResponse(headers, body);
        client.executeMethod(method);
		assertTrue("Connection should be closed", method.shouldCloseConnection(conn));
		assertFalse("Connection should NOT be closed", method.isConnectionCloseForced());
	}
}
