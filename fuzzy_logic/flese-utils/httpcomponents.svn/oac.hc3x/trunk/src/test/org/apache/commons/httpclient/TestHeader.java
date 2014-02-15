/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/oac.hc3x/trunk/src/test/org/apache/commons/httpclient/TestHeader.java $
 * $Revision: 608014 $
 * $Date: 2008-01-02 06:48:53 +0100 (Wed, 02 Jan 2008) $
 * ====================================================================
 *
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
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
 * @version $Id: TestHeader.java 608014 2008-01-02 05:48:53Z rolandw $
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

    public void testToExternalFormNull() {
        Header header = (Header)makePair();
        assertEquals(": \r\n",header.toExternalForm());
    }

    public void testToExternalFormNullName() {
        Header header = (Header)makePair(null,"value");
        assertEquals(": value\r\n",header.toExternalForm());
    }

    public void testToExternalFormNullValue() {
        Header header = (Header)makePair("name",null);
        assertEquals("name: \r\n",header.toExternalForm());
    }

    public void testToExternalForm() {
        Header header = (Header)makePair("a","b");
        assertEquals("a: b\r\n",header.toExternalForm());
    }

    public void testEqualToNVP() {
        NameValuePair header = makePair("a","b");
        NameValuePair pair = new NameValuePair("a","b");
        assertTrue(header.equals(pair));
        assertTrue(pair.equals(header));
    }
}
