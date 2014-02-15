/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/test/java/org/apache/http/conn/TestAllConn.java $
 * $Revision: 555241 $
 * $Date: 2007-07-11 13:05:31 +0200 (Wed, 11 Jul 2007) $
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

package org.apache.http.conn;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class TestAllConn extends TestCase {

    public TestAllConn(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();

        suite.addTest(TestHttpRoute.suite());
        suite.addTest(TestRouteDirector.suite());
        suite.addTest(TestRouteTracker.suite());
        suite.addTest(TestScheme.suite());
        suite.addTest(TestParams.suite());
        suite.addTest(TestExceptions.suite());

        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestAllConn.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
