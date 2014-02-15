/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestHeader.java,v 1.1 2001/08/03 16:57:29 rwaldhoff Exp $
 * $Revision: 1.1 $
 * $Date: 2001-08-03 18:57:29 +0200 (Fri, 03 Aug 2001) $
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
 * Simple tests for {@link NameValuePair}.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestHeader.java 133477 2001-08-03 16:57:29Z rwaldhoff $
 */
public class TestHeader extends TestNVP {

    // ------------------------------------------------------------ Constructor
    public TestHeader(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestHeader.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestHeader.class);
    }

    // ------------------------------------------------------ Protected Methods

    protected NameValuePair makePair() {
        return new Header();
    }

    protected NameValuePair makePair(String name, String value) {
        return new Header(name,value);
    }


    // ----------------------------------------------------------- Test Methods

    public void testToStringNull() {
        NameValuePair header = makePair();
        assertEquals("null: null\r\n",header.toString());
    }

    public void testToString() {
        NameValuePair header = makePair("a","b");
        assertEquals("a: b\r\n",header.toString());
    }

    public void testNotEqualToNVP() {
        NameValuePair header = makePair("a","b");
        NameValuePair pair = new NameValuePair("a","b");
        assert(!header.equals(pair));
        assert(!pair.equals(header));
    }
}
