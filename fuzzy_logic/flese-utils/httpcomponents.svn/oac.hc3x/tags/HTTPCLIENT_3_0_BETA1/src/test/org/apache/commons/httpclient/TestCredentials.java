/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestCredentials.java,v 1.1 2004/10/31 13:46:54 olegk Exp $
 * $Revision: 1.1 $
 * $Date: 2004-10-31 14:46:54 +0100 (Sun, 31 Oct 2004) $
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
 * Unit tests for {@link Credentials}.
 *
 * @author Rodney Waldhoff
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @version $Id: TestCredentials.java 134747 2004-10-31 13:46:54Z olegk $
 */
public class TestCredentials extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestCredentials(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestCredentials.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestCredentials.class);
    }

    public void testCredentialConstructors() {
        try {
            new UsernamePasswordCredentials(null, null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) {
            // expected
        }
        try {
            new NTCredentials("user", "password", null, null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) {
            // expected
        }
        try {
            new NTCredentials("user", "password", "host", null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) {
            // expected
        }
        NTCredentials creds = new NTCredentials("user", null, "host", "domain");
        assertNotNull(creds.getUserName());
        assertNull(creds.getPassword());
        assertNotNull(creds.getDomain());
        assertNotNull(creds.getHost());
    }

}
