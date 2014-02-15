/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/test/java/org/apache/http/impl/entity/TestAllEntityImpl.java $
 * $Revision: 472564 $
 * $Date: 2006-11-08 18:59:20 +0100 (Wed, 08 Nov 2006) $
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

package org.apache.http.impl.entity;

import junit.framework.*;

public class TestAllEntityImpl extends TestCase {

    public TestAllEntityImpl(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestEntitySerializer.suite());
        suite.addTest(TestEntityDeserializer.suite());
        suite.addTest(TestLaxContentLengthStrategy.suite());
        suite.addTest(TestStrictContentLengthStrategy.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestAllEntityImpl.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
