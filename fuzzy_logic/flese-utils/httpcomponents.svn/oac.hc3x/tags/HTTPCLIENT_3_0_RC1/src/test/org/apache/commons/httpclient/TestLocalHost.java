/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestLocalHost.java,v 1.10 2004/11/20 11:16:47 olegk Exp $
 * $Revision: 1.10 $
 * $Date: 2004-11-20 12:16:48 +0100 (Sat, 20 Nov 2004) $
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * A suite composed of only those tests which require the local webserver and
 * test web application.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestLocalHost.java 134774 2004-11-20 11:16:48Z olegk $
 */
public class TestLocalHost extends TestCase {

    public TestLocalHost(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestMethodsLocalHost.suite());
        suite.addTest(TestGetMethodLocal.suite());
        suite.addTest(TestTraceMethodLocal.suite());
        suite.addTest(TestHttpConnectionManager.suite());
        suite.addTest(TestWebapp.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestLocalHost.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
