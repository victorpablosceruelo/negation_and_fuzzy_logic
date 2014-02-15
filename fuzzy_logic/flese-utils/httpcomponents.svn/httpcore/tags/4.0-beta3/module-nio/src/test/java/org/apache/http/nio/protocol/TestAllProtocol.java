/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta3/module-nio/src/test/java/org/apache/http/nio/protocol/TestAllProtocol.java $
 * $Revision: 636277 $
 * $Date: 2008-03-12 11:52:55 +0100 (Wed, 12 Mar 2008) $
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

package org.apache.http.nio.protocol;

import junit.framework.*;

public class TestAllProtocol extends TestCase {

    public TestAllProtocol(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestBufferingNHttpHandlers.suite());
        suite.addTest(TestThrottlingNHttpHandlers.suite());
        suite.addTest(TestNIOSSLHttp.suite());
        suite.addTest(TestAsyncNHttpHandlers.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestAllProtocol.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
