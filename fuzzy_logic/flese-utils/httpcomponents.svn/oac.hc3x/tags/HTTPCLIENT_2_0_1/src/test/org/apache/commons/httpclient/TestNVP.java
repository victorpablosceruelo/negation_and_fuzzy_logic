/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestNVP.java,v 1.4.2.1 2004/02/22 18:21:16 olegk Exp $
 * $Revision: 1.4.2.1 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
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

import junit.framework.*;

/**
 * Simple tests for {@link NameValuePair}.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestNVP.java 134531 2004-02-22 18:21:18Z olegk $
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
        assertTrue(null == pair.getName());
        assertTrue(null == pair.getValue());
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
        assertTrue(!pair1.equals(pair2));
        assertTrue(!pair2.equals(pair1));

        pair2.setName("name");

        assertEquals(pair1,pair1);
        assertEquals(pair1.hashCode(),pair1.hashCode());
        assertEquals(pair2,pair2);
        assertEquals(pair2.hashCode(),pair2.hashCode());
        assertTrue(!pair1.equals(pair2));
        assertTrue(!pair2.equals(pair1));


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
