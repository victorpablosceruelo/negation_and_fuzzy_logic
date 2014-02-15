/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestHttpConnectionManager.java,v 1.4 2003/01/23 22:48:27 jsdever Exp $
 * $Revision: 1.4 $
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
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.GetMethod;

/**
 *
 * Unit tests for {@link HttpConnectionManager}.  These tests
 * do not require any network connection or web app.
 *
 * @author Marc A. Saegesser
 * @version $Id: TestHttpConnectionManager.java 134019 2003-01-23 22:48:49Z jsdever $
 */
public class TestHttpConnectionManager extends TestLocalHostBase {
    // ----------------------------------------------------- Instance Variables

    // ------------------------------------------------------------ Constructor
    public TestHttpConnectionManager(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestHttpConnectionManager.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestHttpConnectionManager.class);
    }


    // ----------------------------------------------------------- Test Methods

    // Test the accessor methods
    public void testMaxConnectionsAccessors() {
        MultiThreadedHttpConnectionManager mgr = new MultiThreadedHttpConnectionManager();

        // First test the default value
        assertEquals("Default MaxConnections", 2, mgr.getMaxConnectionsPerHost());

        mgr.setMaxConnectionsPerHost(10);
        assertEquals("MaxConnections", 10, mgr.getMaxConnectionsPerHost());
    }

    public void testGetConnection() {
        MultiThreadedHttpConnectionManager mgr = new MultiThreadedHttpConnectionManager();

        HostConfiguration hostConfiguration = new HostConfiguration();
        hostConfiguration.setHost("www.nosuchserver.com", 80, "http");

        // Create a new connection
        HttpConnection conn = mgr.getConnection(hostConfiguration);
        // Validate the connection properties
        assertEquals("Host", "www.nosuchserver.com", conn.getHost());
        assertEquals("Port", 80, conn.getPort());
        // Release the connection
        mgr.releaseConnection(conn);

        // Create a new connection
        hostConfiguration.setHost("www.nosuchserver.com", -1, "https");
        conn = mgr.getConnection(hostConfiguration);
        // Validate the connection properties
        assertEquals("Host", "www.nosuchserver.com", conn.getHost());
        assertEquals("Port", 443, conn.getPort());
        // Release the connection
        mgr.releaseConnection(conn);

        // Create a new connection
        hostConfiguration.setHost("www.nowhere.org", 8080, "http");
        conn = mgr.getConnection(hostConfiguration);
        // Validate the connection properties
        assertEquals("Host", "www.nowhere.org", conn.getHost());
        assertEquals("Port", 8080, conn.getPort());
        // Release the connection
        mgr.releaseConnection(conn);

    }

    public void testReleaseConnection() {

        MultiThreadedHttpConnectionManager connectionManager = new MultiThreadedHttpConnectionManager();
        connectionManager.setMaxConnectionsPerHost(1);

        HttpClient client = new HttpClient(connectionManager);
        client.getHostConfiguration().setHost(host, port, "http");
        // we shouldn't have to wait if a connection is available
        client.setHttpConnectionFactoryTimeout( 1 );

        GetMethod getMethod = new GetMethod("/");
        getMethod.setFollowRedirects(true);

        try {
            client.executeMethod(getMethod);
        } catch (Exception e) {
            fail("error reading from server: " + e);
        }

        try {
            // this should fail quickly since the connection has not been released
            client.executeMethod(getMethod);
            fail("a httpConnection should not be available");
        } catch (HttpException e) {            
        } catch (IOException e) {
            fail("error reading from server; " + e);
        }

        // this should release the connection
        getMethod.releaseConnection();

        getMethod = new GetMethod("/");
        getMethod.setFollowRedirects(true);

        try {
            // this should fail quickly if the connection has not been released
            client.executeMethod(getMethod);
        } catch (HttpException e) {
            fail("httpConnection does not appear to have been released: " + e);
        } catch (IOException e) {
            fail("error reading from server; " + e);
        }

    }

    /**
     * Makes sure that a connection gets released after the content of the body
     * is read.
     */
    public void testResponseAutoRelease() {

        MultiThreadedHttpConnectionManager connectionManager = new MultiThreadedHttpConnectionManager();
        connectionManager.setMaxConnectionsPerHost(1);

        HttpClient client = new HttpClient(connectionManager);
        client.getHostConfiguration().setHost(host, port, "http");
        // we shouldn't have to wait if a connection is available
        client.setHttpConnectionFactoryTimeout( 1 );

        GetMethod getMethod = new GetMethod("/");
        getMethod.setFollowRedirects(true);

        try {
            client.executeMethod(getMethod);
        } catch (Exception e) {
            fail("error reading from server: " + e);
        }
        
        // this should release the connection
        getMethod.getResponseBody();

        getMethod = new GetMethod("/");
        getMethod.setFollowRedirects(true);

        try {
            // this should fail quickly if the connection has not been released
            client.executeMethod(getMethod);
        } catch (HttpException e) {
            fail("httpConnection does not appear to have been released: " + e);
        } catch (IOException e) {
            fail("error reading from server; " + e);
        }

    }
    
    public void testMaxConnectionsPerServer() {
     
        MultiThreadedHttpConnectionManager connectionManager = new MultiThreadedHttpConnectionManager();
        connectionManager.setMaxConnectionsPerHost(1);

        HttpClient client = new HttpClient(connectionManager);
        client.getHostConfiguration().setHost(host, port, "http");
        // we shouldn't have to wait if a connection is available
        client.setHttpConnectionFactoryTimeout( 1 );

        GetMethod getMethod = new GetMethod("/");
        getMethod.setFollowRedirects(true);

        try {
            client.executeMethod(getMethod);
        } catch (Exception e) {
            fail("error reading from server: " + e);
        }

        GetMethod getMethod2 = new GetMethod("/");
        getMethod2.setFollowRedirects(true);

        try {
            // this should fail quickly since the connection has not been released
            client.executeMethod(getMethod2);
            fail("a httpConnection should not be available");
        } catch (HttpException e) {
        } catch (IOException e) {
            fail("error reading from server; " + e);
        }
                
    }
    
    public void testReclaimUnusedConnection() {

        MultiThreadedHttpConnectionManager connectionManager = new MultiThreadedHttpConnectionManager();
        connectionManager.setMaxConnectionsPerHost(1);

        HttpClient client = new HttpClient(connectionManager);
        client.getHostConfiguration().setHost(host, port, "http");
        // we shouldn't have to wait if a connection is available
        client.setHttpConnectionFactoryTimeout( 30000 );

        GetMethod getMethod = new GetMethod("/");
        getMethod.setFollowRedirects(true);

        try {
            client.executeMethod(getMethod);
        } catch (Exception e) {
            fail("error reading from server: " + e);
        }

        getMethod = new GetMethod("/");
        getMethod.setFollowRedirects(true);
        
        Runtime.getRuntime().gc();

        try {
            // we didn't explicitly release the connection, but it should be 
            // reclaimed by the garbage collector, we hope:)
            client.executeMethod(getMethod);
        } catch (HttpException e) {
            fail("httpConnection does not appear to have been reclaimed by the GC: " + e);
        } catch (IOException e) {
            fail("error reading from server; " + e);
        }

    }
    
    public void testGetMultipleConnections() {
        HttpConnectionManager mgr = new MultiThreadedHttpConnectionManager();

        HostConfiguration hostConfig1 = new HostConfiguration();
        hostConfig1.setHost("www.nosuchserver.com", 80, "http");
        
        HostConfiguration hostConfig2 = new HostConfiguration();
        hostConfig2.setHost("www.nosuchserver.com", -1, "http");

        HostConfiguration hostConfig3 = new HostConfiguration();
        hostConfig3.setHost("www.nosuchserver.com", -1, "http");

        // Create a new connection
        HttpConnection conn1 = mgr.getConnection(hostConfig1);
        // Release the connection
        mgr.releaseConnection(conn1);

        // Get the same connection again
        HttpConnection conn2 = mgr.getConnection(hostConfig2);
        assertEquals("Same connection", conn1, conn2);
        // don't release yet

        // Get another new connection
        HttpConnection conn3 = mgr.getConnection(hostConfig3);
        assertTrue(conn2 != conn3);

    }

    public void testTimeout()
    {
        MultiThreadedHttpConnectionManager mgr = new MultiThreadedHttpConnectionManager();
        
        try{
            HostConfiguration hostConfig = new HostConfiguration();
            hostConfig.setHost("www.nosuchserver.com", 80, "http");
            
            HttpConnection conn1 = mgr.getConnection(hostConfig);
            HttpConnection conn2 = mgr.getConnection(hostConfig);
            HttpConnection conn3 = mgr.getConnection(hostConfig, 5000);
            fail("Expected an HttpException.");
            
        }catch(HttpException e){
            //Expected result
        }
    }
}

