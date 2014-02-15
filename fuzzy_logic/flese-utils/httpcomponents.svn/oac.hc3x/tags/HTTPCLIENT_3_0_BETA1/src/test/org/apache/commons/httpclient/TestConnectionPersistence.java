/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestConnectionPersistence.java,v 1.1 2004/11/07 12:31:42 olegk Exp $
 * $Revision: 1.1 $
 * $Date: 2004-11-07 13:31:42 +0100 (Sun, 07 Nov 2004) $
 * ====================================================================
 *
 *  Copyright 2002-2004 The Apache Software Foundation
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
 */

package org.apache.commons.httpclient;

import java.io.IOException;

import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Connection persistence tests
 * 
 * @author Oleg Kalnichevski
 *
 * @version $Id: TestConnectionPersistence.java 134763 2004-11-07 12:31:42Z olegk $
 */
public class TestConnectionPersistence extends HttpClientTestBase {
    
    // ------------------------------------------------------------ Constructor
    public TestConnectionPersistence(final String testName) throws IOException {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestConnectionPersistence.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestConnectionPersistence.class);
    }

    // ----------------------------------------------------------- Test Methods

    public void testConnPersisenceHTTP10() throws Exception {
        this.server.setHttpService(new EchoService());

        AccessibleHttpConnectionManager connman = new AccessibleHttpConnectionManager();
        
        this.client.getParams().setVersion(HttpVersion.HTTP_1_0);
        this.client.setHttpConnectionManager(connman);
        
        PostMethod httppost = new PostMethod("/test/");
        httppost.setRequestEntity(new StringRequestEntity("stuff"));
        try {
            this.client.executeMethod(httppost);
        } finally {
            httppost.releaseConnection();
        }
        assertFalse(connman.getConection().isOpen());

        httppost = new PostMethod("/test/");
        httppost.setRequestEntity(new StringRequestEntity("more stuff"));
        try {
            this.client.executeMethod(httppost);
        } finally {
            httppost.releaseConnection();
        }
        assertFalse(connman.getConection().isOpen());
    }

    public void testConnPersisenceHTTP11() throws Exception {
        this.server.setHttpService(new EchoService());

        AccessibleHttpConnectionManager connman = new AccessibleHttpConnectionManager();
        
        this.client.getParams().setVersion(HttpVersion.HTTP_1_1);
        this.client.setHttpConnectionManager(connman);
        
        PostMethod httppost = new PostMethod("/test/");
        httppost.setRequestEntity(new StringRequestEntity("stuff"));
        try {
            this.client.executeMethod(httppost);
        } finally {
            httppost.releaseConnection();
        }
        assertTrue(connman.getConection().isOpen());

        httppost = new PostMethod("/test/");
        httppost.setRequestEntity(new StringRequestEntity("more stuff"));
        try {
            this.client.executeMethod(httppost);
        } finally {
            httppost.releaseConnection();
        }
        assertTrue(connman.getConection().isOpen());
    }

    public void testConnClose() throws Exception {
        this.server.setHttpService(new EchoService());

        AccessibleHttpConnectionManager connman = new AccessibleHttpConnectionManager();
        
        this.client.getParams().setVersion(HttpVersion.HTTP_1_1);
        this.client.setHttpConnectionManager(connman);
        
        PostMethod httppost = new PostMethod("/test/");
        httppost.setRequestEntity(new StringRequestEntity("stuff"));
        try {
            this.client.executeMethod(httppost);
        } finally {
            httppost.releaseConnection();
        }
        assertTrue(connman.getConection().isOpen());

        httppost = new PostMethod("/test/");
        httppost.setRequestHeader("Connection", "close");
        httppost.setRequestEntity(new StringRequestEntity("more stuff"));
        try {
            this.client.executeMethod(httppost);
        } finally {
            httppost.releaseConnection();
        }
        assertFalse(connman.getConection().isOpen());
    }

    public void testConnKeepAlive() throws Exception {
        this.server.setHttpService(new EchoService());

        AccessibleHttpConnectionManager connman = new AccessibleHttpConnectionManager();
        
        this.client.getParams().setVersion(HttpVersion.HTTP_1_0);
        this.client.setHttpConnectionManager(connman);
        
        PostMethod httppost = new PostMethod("/test/");
        httppost.setRequestEntity(new StringRequestEntity("stuff"));
        try {
            this.client.executeMethod(httppost);
        } finally {
            httppost.releaseConnection();
        }
        assertFalse(connman.getConection().isOpen());

        httppost = new PostMethod("/test/");
        httppost.setRequestHeader("Connection", "keep-alive");
        httppost.setRequestEntity(new StringRequestEntity("more stuff"));
        try {
            this.client.executeMethod(httppost);
        } finally {
            httppost.releaseConnection();
        }
        assertTrue(connman.getConection().isOpen());
    }

}

