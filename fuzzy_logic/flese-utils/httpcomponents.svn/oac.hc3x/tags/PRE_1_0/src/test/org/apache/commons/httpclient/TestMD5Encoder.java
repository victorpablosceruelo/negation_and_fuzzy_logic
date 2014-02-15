/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestMD5Encoder.java,v 1.1 2001/08/07 17:42:21 rwaldhoff Exp $
 * $Revision: 1.1 $
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
import java.util.Random;

/**
 * Unit tests for {@link MD5Encoder}.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestMD5Encoder.java 133479 2001-08-07 17:42:21Z rwaldhoff $
 */
public class TestMD5Encoder extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestMD5Encoder(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestMD5Encoder.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestMD5Encoder.class);
    }

    // ----------------------------------------------------------- Test Methods

    private MD5Encoder encoder = new MD5Encoder();
    private Random random = new Random();

    public void testEncode() {
        for(int i=0;i<100;i++) {
            byte[] data = new byte[16];
            random.nextBytes(data);
            StringBuffer buf = new StringBuffer();
            for(int j=0;j<data.length;j++) {
                String hexint = Integer.toHexString((int)data[j]);
                if(hexint.length() == 1) {
                    hexint = "0" + hexint;
                }
                buf.append(hexint.substring(hexint.length() - 2));
            }
            assertEquals(buf.toString(),encoder.encode(data));
        }
    }

    public void testEdges() {
        try {
            encoder.encode(null);
            fail("Should have thrown a NullPointerException");
        } catch(NullPointerException e) {
        }
        byte[] toolong = new byte[17];
        byte[] tooshort = new byte[15];
        assert(null == encoder.encode(tooshort));
        assert(null == encoder.encode(toolong));
    }
}
