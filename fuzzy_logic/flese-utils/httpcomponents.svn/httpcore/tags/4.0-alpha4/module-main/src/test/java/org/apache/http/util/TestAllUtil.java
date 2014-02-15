/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-main/src/test/java/org/apache/http/util/TestAllUtil.java $
 * $Revision: 505893 $
 * $Date: 2007-02-11 12:31:59 +0100 (Sun, 11 Feb 2007) $
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

package org.apache.http.util;

import junit.framework.*;

public class TestAllUtil extends TestCase {

    public TestAllUtil(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestLangUtils.suite());
        suite.addTest(TestExceptionUtils.suite());
        suite.addTest(TestEncodingUtils.suite());
        suite.addTest(TestDateUtils.suite());
        suite.addTest(TestEntityUtils.suite());
        suite.addTest(TestHeaderUtils.suite());
        suite.addTest(TestHeaderGroup.suite());
        suite.addTest(TestByteArrayBuffer.suite());
        suite.addTest(TestCharArrayBuffer.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestAllUtil.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
