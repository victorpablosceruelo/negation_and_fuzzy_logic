/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestWebapp.java,v 1.15 2004/11/20 17:56:39 olegk Exp $
 * $Revision: 192986 $
 * $Date: 2005-06-22 21:51:04 +0200 (Wed, 22 Jun 2005) $
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
 * This suite of tests depends upon the httpclienttest webapp,
 * which is available in the httpclient/src/test-webapp
 * directory in the CVS tree.
 * <p>
 * The webapp should be deployed in the context "httpclienttest"
 * on a servlet engine running on port 8080 on the localhost
 * (IP 127.0.0.1).
 * <p>
 * You can change the assumed port by setting the
 * "httpclient.test.localPort" property.
 * You can change the assumed host by setting the
 * "httpclient.test.localHost" property.
 * You can change the assumed context by setting the
 * "httpclient.test.webappContext" property.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestWebapp.java 192986 2005-06-22 19:51:04Z olegk $
 */
public class TestWebapp extends TestCase {

    public TestWebapp(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestWebappMethods.suite());
        suite.addTest(TestWebappParameters.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestWebapp.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}

