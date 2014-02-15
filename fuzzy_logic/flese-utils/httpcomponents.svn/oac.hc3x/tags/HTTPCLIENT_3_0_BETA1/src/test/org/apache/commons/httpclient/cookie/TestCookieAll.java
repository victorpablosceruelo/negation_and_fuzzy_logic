/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/cookie/TestCookieAll.java,v 1.2 2004/04/25 12:25:09 olegk Exp $
 * $Revision: 1.2 $
 * $Date: 2004-04-25 14:25:09 +0200 (Sun, 25 Apr 2004) $
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
 */

package org.apache.commons.httpclient.cookie;

import junit.framework.*;

/**
 * @author oleg Kalnichevski 
 * 
 * @version $Id: TestCookieAll.java 134578 2004-04-25 12:25:09Z olegk $
 */
public class TestCookieAll extends TestCase {

    public TestCookieAll(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestCookie.suite());
        suite.addTest(TestCookieCompatibilitySpec.suite());
        suite.addTest(TestCookieRFC2109Spec.suite());
        suite.addTest(TestCookieNetscapeDraft.suite());
        suite.addTest(TestCookieIgnoreSpec.suite());
        suite.addTest(TestCookiePolicy.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestCookieAll.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
