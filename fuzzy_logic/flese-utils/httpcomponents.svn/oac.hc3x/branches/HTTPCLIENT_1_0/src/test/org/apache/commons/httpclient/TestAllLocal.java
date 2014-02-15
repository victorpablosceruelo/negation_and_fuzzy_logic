/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestAllLocal.java,v 1.4 2001/08/07 17:42:21 rwaldhoff Exp $
 * $Revision: 1.4 $
 * $Date: 2001-08-07 19:42:21 +0200 (Tue, 07 Aug 2001) $
 * ====================================================================
 * Copyright (C) The Apache Software Foundation. All rights reserved.
 *
 * This software is published under the terms of the Apache Software License
 * version 1.1, a copy of which has been included with this distribution in
 * the LICENSE file.
 */

package org.apache.commons.httpclient;

import junit.framework.*;

/**
 * @author Rodney Waldhoff
 * @version $Id: TestAllLocal.java 133479 2001-08-07 17:42:21Z rwaldhoff $
 */
public class TestAllLocal extends TestCase {

    public TestAllLocal(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestBase64.suite());
        suite.addTest(TestCookie.suite());
        suite.addTest(TestMethodsLocalHost.suite());
        suite.addTest(TestNVP.suite());
        suite.addTest(TestHeader.suite());
        suite.addTest(TestMD5Encoder.suite());
        suite.addTest(TestAuthenticator.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestAllLocal.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
