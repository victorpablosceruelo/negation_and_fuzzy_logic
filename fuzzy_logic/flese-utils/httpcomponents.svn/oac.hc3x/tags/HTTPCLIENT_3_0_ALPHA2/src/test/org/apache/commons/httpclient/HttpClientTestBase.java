/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/HttpClientTestBase.java,v 1.2 2004/02/27 19:01:33 olegk Exp $
 * $Revision: 1.2 $
 * $Date: 2004-02-27 20:01:34 +0100 (Fri, 27 Feb 2004) $
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
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.httpclient.server.SimpleHttpServer;

/**
 * Base class for test cases using 
 * {@link org.apache.commons.httpclient.server.SimpleHttpServer} based 
 * testing framework.
 *
 * @author Oleg Kalnichevski
 * 
 * @version $Id: HttpClientTestBase.java 134539 2004-02-27 19:01:34Z olegk $
 */
public class HttpClientTestBase extends TestCase {

    protected HttpClient client = null;
    protected SimpleHttpServer server = null;

    // ------------------------------------------------------------ Constructor
    public HttpClientTestBase(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { HttpClientTestBase.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(HttpClientTestBase.class);
    }

    // ------------------------------------------------- TestCase setup/shutdown

    public void setUp() throws IOException {
        // configure the server
        this.server = new SimpleHttpServer(); // use arbitrary port

        // configure the client
        this.client = new HttpClient();
        this.client.getHostConfiguration().setHost(
            this.server.getLocalAddress(), 
            this.server.getLocalPort(),
            Protocol.getProtocol("http"));
    
    }

    public void tearDown() throws IOException {
        this.client = null;
        this.server.destroy();
        this.server = null;
    }
}
