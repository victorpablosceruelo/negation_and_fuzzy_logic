/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestWebappHeaders.java,v 1.7 2003/01/23 22:48:28 jsdever Exp $
 * $Revision: 1.7 $
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

import java.net.InetAddress;
import junit.framework.*;
import org.apache.commons.httpclient.methods.*;

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
 * @version $Id: TestWebappHeaders.java 134019 2003-01-23 22:48:49Z jsdever $
 */
public class TestWebappHeaders extends TestWebappBase {

    public TestWebappHeaders(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestWebappHeaders.class);
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestWebappHeaders.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------------------ Tests

    /**
     * Test {@link HttpMethod#addRequestHeader}.
     */
    public void testAddRequestHeader() throws Exception {
        HttpClient client = new HttpClient();
	    client.getHostConfiguration().setHost(host, port, "http");
        GetMethod method = new GetMethod("/" + context + "/headers");
        method.setRequestHeader(new Header("addRequestHeader(Header)","True"));
        method.setRequestHeader("addRequestHeader(String,String)","Also True");
        method.setUseDisk(false);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        // Tomcat 4 at least converts the header name to lower case
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"addrequestheader(header)\";value=\"True\"<br>") >= 0);
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"addrequestheader(string,string)\";value=\"Also True\"<br>") >= 0);
    }

    /**
     * Test {@link HttpMethod#removeRequestHeader}.
     */
    public void testRemoveRequestHeader() throws Exception {
        HttpClient client = new HttpClient();
	    client.getHostConfiguration().setHost(host, port, "http");
        GetMethod method = new GetMethod("/" + context + "/headers");
        method.setRequestHeader(new Header("XXX-A-HEADER","true"));
        method.removeRequestHeader("XXX-A-HEADER");
        method.setUseDisk(false);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        // Tomcat 4 at least converts the header name to lower case
        assertTrue(!(method.getResponseBodyAsString().indexOf("xxx-a-header") >= 0));
    }

    /**
     * Test {@link HttpMethod#addRequestHeader}.
     */
    public void testOverwriteRequestHeader() throws Exception {
        HttpClient client = new HttpClient();
	    client.getHostConfiguration().setHost(host, port, "http");
        GetMethod method = new GetMethod("/" + context + "/headers");
        method.setRequestHeader(new Header("xxx-a-header","one"));
        method.setRequestHeader("XXX-A-HEADER","two");
        method.setUseDisk(false);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        // Tomcat 4 at least converts the header name to lower case
        assertTrue(method.getResponseBodyAsString().indexOf("name=\"xxx-a-header\";value=\"two\"<br>") >= 0);
    }

    /**
     * Test {@link HttpMethod#getResponseHeader}.
     */
    public void testGetResponseHeader() throws Exception {
        HttpClient client = new HttpClient();
	    client.getHostConfiguration().setHost(host, port, "http");
        GetMethod method = new GetMethod("/" + context + "/headers");
        method.setUseDisk(false);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        Header h = new Header("HeaderSetByServlet","Yes");
        assertEquals(h,method.getResponseHeader("headersetbyservlet"));
    }

    /**
     * Test {@link HttpMethodBase.addHostRequestHeader}.
     */
    public void testHostRequestHeader() throws Exception {
        InetAddress addr = InetAddress.getByName(host);
        String ip = addr.getHostAddress();
        String hostname = addr.getHostName();

        HttpClient client = new HttpClient();
        GetMethod get = new GetMethod("/" + context);

        // Open connection using IP.  Host header should be sent
        // Note: RFC 2616 is somewhat unclear on whether a host should
        // be sent in this context - however, both Mozilla and IE send
        // the header for an IP address, instead of sending blank.
	    client.getHostConfiguration().setHost(ip, port, "http");
        try {
            client.executeMethod(get);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        Header hostHeader = get.getRequestHeader("Host");
        assertTrue(hostHeader != null);

        // reset 
        get.recycle();
        get.setPath("/" + context);

        // Open connection using Host.  Host header should
        // contain this value (this test will fail if DNS
        // is not available. Additionally, if the port is
        // something other that 80, then the port value
        // should also be present in the header.
	    client.getHostConfiguration().setHost(hostname, port, "http");
        try {
            client.executeMethod(get);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        hostHeader = get.getRequestHeader("Host");
        assertTrue(hostHeader != null);
        if (port == 80) {
            // no port information should be in the value
            assertTrue(hostHeader.getValue().equals(hostname));
        } else {
            assertTrue(hostHeader.getValue().equals(hostname + ":" + port));
        }
    }

}

