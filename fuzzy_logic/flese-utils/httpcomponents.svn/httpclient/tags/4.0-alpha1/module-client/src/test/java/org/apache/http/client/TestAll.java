/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/test/java/org/apache/http/client/TestAll.java $
 * $Revision: 548037 $
 * $Date: 2007-06-17 14:40:50 +0200 (Sun, 17 Jun 2007) $
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.http.client;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.http.conn.TestAllConn;
import org.apache.http.conn.ssl.TestAllSSL;
import org.apache.http.cookie.TestAllCookie;
import org.apache.http.impl.client.TestAllHttpClientImpl;
import org.apache.http.impl.conn.TestAllConnImpl;
import org.apache.http.impl.cookie.TestAllCookieImpl;

public class TestAll extends TestCase {

    public TestAll(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestAllCookie.suite());
        suite.addTest(TestAllCookieImpl.suite());
        suite.addTest(TestAllHttpClientImpl.suite());
        suite.addTest(TestAllConn.suite());
        suite.addTest(TestAllConnImpl.suite());
        suite.addTest(TestAllSSL.suite());        
        suite.addTest(TestHttpState.suite());        
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestAll.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
