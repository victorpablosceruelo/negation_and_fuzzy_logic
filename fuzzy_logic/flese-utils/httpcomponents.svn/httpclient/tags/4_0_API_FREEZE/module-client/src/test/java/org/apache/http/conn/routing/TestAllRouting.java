/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4_0_API_FREEZE/module-client/src/test/java/org/apache/http/conn/routing/TestAllRouting.java $
 * $Revision: 613654 $
 * $Date: 2008-01-20 20:00:19 +0100 (Sun, 20 Jan 2008) $
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

package org.apache.http.conn.routing;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class TestAllRouting extends TestCase {

    public TestAllRouting(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();

        suite.addTest(TestHttpRoute.suite());
        suite.addTest(TestRouteDirector.suite());
        suite.addTest(TestRouteTracker.suite());

        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestAllRouting.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
