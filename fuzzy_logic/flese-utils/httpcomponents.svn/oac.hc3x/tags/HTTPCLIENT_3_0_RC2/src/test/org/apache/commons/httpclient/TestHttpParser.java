/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestHttpParser.java,v 1.3 2004/02/22 18:08:49 olegk Exp $
 * $Revision: 155418 $
 * $Date: 2005-02-26 14:01:52 +0100 (Sat, 26 Feb 2005) $
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

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import junit.framework.*;

/**
 * Simple tests for {@link HttpParser}.
 *
 * @author Oleg Kalnichevski
 * @version $Id: TestHttpParser.java 155418 2005-02-26 13:01:52Z dirkv $
 */
public class TestHttpParser extends TestCase {

    private static final String HTTP_ELEMENT_CHARSET = "US-ASCII";

    // ------------------------------------------------------------ Constructor
    public TestHttpParser(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestHeader.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestHttpParser.class);
    }

    public void testReadHttpLine() throws Exception {
        InputStream instream = new ByteArrayInputStream(
            "\r\r\nstuff\r\n".getBytes(HTTP_ELEMENT_CHARSET)); 
        assertEquals("\r", HttpParser.readLine(instream, HTTP_ELEMENT_CHARSET));
        assertEquals("stuff", HttpParser.readLine(instream, HTTP_ELEMENT_CHARSET));
        assertEquals(null, HttpParser.readLine(instream, HTTP_ELEMENT_CHARSET));

        instream = new ByteArrayInputStream(
            "\n\r\nstuff\r\n".getBytes("US-ASCII")); 
        assertEquals("", HttpParser.readLine(instream, HTTP_ELEMENT_CHARSET));
        assertEquals("", HttpParser.readLine(instream, HTTP_ELEMENT_CHARSET));
        assertEquals("stuff", HttpParser.readLine(instream, HTTP_ELEMENT_CHARSET));
        assertEquals(null, HttpParser.readLine(instream, HTTP_ELEMENT_CHARSET));
    }
}
