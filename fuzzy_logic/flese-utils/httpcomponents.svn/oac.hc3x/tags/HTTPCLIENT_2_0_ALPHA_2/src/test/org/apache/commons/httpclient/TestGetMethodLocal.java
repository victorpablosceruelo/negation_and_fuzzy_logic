/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestGetMethodLocal.java,v 1.8 2003/01/23 22:48:25 jsdever Exp $
 * $Revision: 1.8 $
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

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.GetMethod;

/**
 * Simple tests of {@link GetMethod} hitting
 * a local webserver.
 * <p>
 * This test suite assumes a webserver is running on
 * port 8080 on the 127.0.0.1 (localhost) host.  It
 * further assumes that this webserver will respond
 * to an HTTP GET of <tt>/</tt> with a 200 response.
 * <p>
 * You can change the assumed port by setting the
 * "httpclient.test.localPort" property.
 * You can change the assumed host by setting the
 * "httpclient.test.localHost" property.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestGetMethodLocal.java 134019 2003-01-23 22:48:49Z jsdever $
 */
public class TestGetMethodLocal extends TestCase {


    // -------------------------------------------------------------- Environment

    public static String getTestHost() {
        return System.getProperty("httpclient.test.localHost","127.0.0.1");
    }

    public static int getTestPort() {
        String portString = System.getProperty("httpclient.test.localPort","8080");
        int tempPort = 8080;
        try {
            tempPort = Integer.parseInt(portString);
        } catch(Exception e) {
            tempPort = 8080;
        }
        return tempPort;

    }

    public static String getTestContext() {
        return System.getProperty(CONTEXT_PROPERTY);
    }
    // -------------------------------------------------------------- Constants

    private static final String host = getTestHost();
    private static final String webAppContext = getTestContext();
    private static final int port = getTestPort();
    private static final String CONTEXT_PROPERTY = "httpclient.test.webappContext";

    // ------------------------------------------------------------ Constructor


    public TestGetMethodLocal(String testName) {
        super(testName);
    }


    // ------------------------------------------------------- TestCase Methods


    public static Test suite() {
        return new TestSuite(TestGetMethodLocal.class);
    }


    // ------------------------------------------------------------------ Tests

    public void testWebappContextPropertySet() {
        assertNotNull(
            "The property \"" + CONTEXT_PROPERTY + "\" should be set",
            getTestContext());
    }

    public void testGetSlashWithoutDisk() {
        HttpClient client = new HttpClient();
        client.getHostConfiguration().setHost(host, port, "http");

        GetMethod method = new GetMethod("/");
        method.setUseDisk(false);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            assertTrue("No data returned.",(data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
    }

    public void testGetSlashWithDisk() {
        HttpClient client = new HttpClient();
        client.getHostConfiguration().setHost(host, port, "http");

        GetMethod method = new GetMethod("/");
        method.setUseDisk(true);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            assertTrue("No data returned.",(data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());
    }

    public void testRecycle() {
        HttpClient client = new HttpClient();
        client.getHostConfiguration().setHost(host, port, "http");

        GetMethod method = new GetMethod("/");
        method.setUseDisk(false);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            assertTrue("No data returned.",(data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());

        method.recycle();
        method.setPath("/");

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            assertTrue("No data returned.",(data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(200,method.getStatusCode());

    }

    public void test404() {
        HttpClient client = new HttpClient();
        client.getHostConfiguration().setHost(host, port, "http");

        GetMethod method = new GetMethod("/i/am/assuming/this/path/and/file/doesnt/exist/on/the/web/server.xyzzy");
        method.setUseDisk(false);
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
        assertEquals(404,method.getStatusCode());

    }

    /**
     * The intent of this test is to allow for the incomplete parsing of a GET
     * response, and to make it particularly tricky, the GET response issues
     * a Connection: close".
     *
     * <p>This wants to insure that a recoverable exception is not unexpectedly
     * triggered.</p>
     */
    public void testGetResponseNotReadAutoRecover() {

        HttpClient client = new HttpClient();
        client.getHostConfiguration().setHost(host, port, "http");

        try {
            // issue a GET with a connection: close, and don't parse the body.
            String path = "/" + webAppContext + "/body";
            GetMethod method1 = new GetMethod(path);
            method1.addRequestHeader("Connection", "close");
            client.executeMethod(method1);
            assertEquals(0, method1.getRecoverableExceptionCount() );

            // issue another GET.
            GetMethod method2 = new GetMethod(path);
            client.executeMethod(method2);
            assertEquals(0, method2.getRecoverableExceptionCount() );
        }
        catch (IOException ioe) {

            fail("Problem executing method : " + ioe.toString() );
        }
    }

}
