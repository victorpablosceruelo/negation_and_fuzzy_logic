/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha2/module-client/src/test/java/org/apache/http/impl/cookie/TestAllCookieImpl.java $
 * $Revision: 581953 $
 * $Date: 2007-10-04 17:53:50 +0200 (Thu, 04 Oct 2007) $
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

package org.apache.http.impl.cookie;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class TestAllCookieImpl extends TestCase {

    public TestAllCookieImpl(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestDateUtils.suite());
        suite.addTest(TestAbstractCookieSpec.suite());
        suite.addTest(TestBasicCookieAttribHandlers.suite());
        suite.addTest(TestNetscapeCookieAttribHandlers.suite());
        suite.addTest(TestRFC2109CookieAttribHandlers.suite());
        suite.addTest(TestBrowserCompatSpec.suite());
        suite.addTest(TestCookieNetscapeDraft.suite());
        suite.addTest(TestCookieRFC2109Spec.suite());
        suite.addTest(TestCookieRFC2965Spec.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestAllCookieImpl.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
