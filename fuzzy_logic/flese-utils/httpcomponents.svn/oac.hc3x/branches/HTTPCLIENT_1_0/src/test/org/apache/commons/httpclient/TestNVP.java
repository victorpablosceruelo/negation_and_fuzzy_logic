/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestNVP.java,v 1.1 2001/08/03 16:57:29 rwaldhoff Exp $
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
 * @version $Id: TestNVP.java 133477 2001-08-03 16:57:29Z rwaldhoff $
 */
public class TestNVP extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestNVP(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestNVP.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestNVP.class);
    }

    // ------------------------------------------------------ Protected Methods

    protected NameValuePair makePair() {
        return new NameValuePair();
    }

    protected NameValuePair makePair(String name, String value) {
        return new NameValuePair(name,value);
    }


    // ----------------------------------------------------------- Test Methods

    public void testGet() {
        NameValuePair pair = makePair("name 1","value 1");
        assertEquals("name 1",pair.getName());
        assertEquals("value 1",pair.getValue());
    }

    public void testSet() {
        NameValuePair pair = makePair();
        assert(null == pair.getName());
        assert(null == pair.getValue());
        pair.setName("name");
        assertEquals("name",pair.getName());
        pair.setValue("value");
        assertEquals("value",pair.getValue());
    }

    public void testEqualsAndHashCode() {
        NameValuePair pair1 = makePair();
        NameValuePair pair2 = makePair();

        assertEquals(pair1,pair1);
        assertEquals(pair1.hashCode(),pair1.hashCode());
        assertEquals(pair2,pair2);
        assertEquals(pair2.hashCode(),pair2.hashCode());
        assertEquals(pair1,pair2);
        assertEquals(pair1.hashCode(),pair2.hashCode());
        assertEquals(pair2,pair1);

        pair1.setName("name");
        pair1.setValue("value");

        assertEquals(pair1,pair1);
        assertEquals(pair1.hashCode(),pair1.hashCode());
        assert(!pair1.equals(pair2));
        assert(!pair2.equals(pair1));

        pair2.setName("name");

        assertEquals(pair1,pair1);
        assertEquals(pair1.hashCode(),pair1.hashCode());
        assertEquals(pair2,pair2);
        assertEquals(pair2.hashCode(),pair2.hashCode());
        assert(!pair1.equals(pair2));
        assert(!pair2.equals(pair1));


        pair2.setValue("value");

        assertEquals(pair1,pair1);
        assertEquals(pair1.hashCode(),pair1.hashCode());
        assertEquals(pair2,pair2);
        assertEquals(pair2.hashCode(),pair2.hashCode());
        assertEquals(pair1,pair2);
        assertEquals(pair1.hashCode(),pair2.hashCode());
        assertEquals(pair2,pair1);
    }
}
