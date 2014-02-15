/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/test/java/org/apache/http/entity/TestAllEntity.java $
 * $Revision: 413444 $
 * $Date: 2006-06-11 15:32:04 +0200 (Sun, 11 Jun 2006) $
 * ====================================================================
 *
 *  Copyright 1999-2006 The Apache Software Foundation
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
 */

package org.apache.http.entity;

import junit.framework.*;

public class TestAllEntity extends TestCase {

    public TestAllEntity(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestAbstractHttpEntity.suite());
        suite.addTest(TestStringEntity.suite());
        suite.addTest(TestByteArrayEntity.suite());
        suite.addTest(TestInputStreamEntity.suite());
        suite.addTest(TestFileEntity.suite());
        suite.addTest(TestBasicHttpEntity.suite());
        suite.addTest(TestHttpEntityWrapper.suite());
        suite.addTest(TestBufferedHttpEntity.suite());
        suite.addTest(TestEntityTemplate.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestAllEntity.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
