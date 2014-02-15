/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-main/src/test/java/org/apache/http/message/TestHttpVersion.java $
 * $Revision: 503402 $
 * $Date: 2007-02-04 13:59:05 +0100 (Sun, 04 Feb 2007) $
 * 
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.http.message;

import org.apache.http.HttpVersion;
import org.apache.http.ProtocolException;
import org.apache.http.util.CharArrayBuffer;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Test cases for {@link BasicHttpVersionFormat BasicHttpVersionFormat}.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public class TestHttpVersion extends TestCase {

    // ------------------------------------------------------------ Constructor

    public TestHttpVersion(String name) {
        super(name);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestHttpVersion.class);
    }

    // ------------------------------------------------------------------ Tests
    
    public void testHttpVersionParsing() throws Exception {
        new HttpVersion(1, 1);
        String s = "HTTP/1.1";
        HttpVersion version = BasicHttpVersionFormat.parse(s);
        assertEquals("HTTP major version number", 1, version.getMajor());
        assertEquals("HTTP minor version number", 1, version.getMinor());
        assertEquals("HTTP version number", s, version.toString());

        s = "HTTP/123.4567";
        version = BasicHttpVersionFormat.parse(s);
        assertEquals("HTTP major version number", 123, version.getMajor());
        assertEquals("HTTP minor version number", 4567, version.getMinor());
        assertEquals("HTTP version number", s, version.toString());
    }

    public void testInvalidHttpVersionParsing() throws Exception {
        try {
            BasicHttpVersionFormat.parse(null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("    ");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("HTT");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("crap");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("HTTP/crap");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("HTTP/1");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("HTTP/1234   ");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("HTTP/1.");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("HTTP/1.1 crap");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("HTTP/whatever.whatever whatever");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
        try {
            BasicHttpVersionFormat.parse("HTTP/1.whatever whatever");
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException e) {
            //expected
        }
    }

    public void testHttpVersionParsingInvalidInput() throws Exception {
        CharArrayBuffer buffer = new CharArrayBuffer(32);
        buffer.append("HTTP/1.1");
        try {
            BasicHttpVersionFormat.parse(null, 0, 0);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            BasicHttpVersionFormat.parse(null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            BasicHttpVersionFormat.parse(buffer, -1, 0);
            fail("IllegalArgumentException should have been thrown");
        } catch (IndexOutOfBoundsException ex) {
            // expected
        }
        try {
            BasicHttpVersionFormat.parse(buffer, 0, 1000);
            fail("IllegalArgumentException should have been thrown");
        } catch (IndexOutOfBoundsException ex) {
            // expected
        }
        try {
            BasicHttpVersionFormat.parse(buffer, 2, 1);
            fail("IllegalArgumentException should have been thrown");
        } catch (IndexOutOfBoundsException ex) {
            // expected
        }
    }

    public void testHttpVersionFormatting() throws Exception {
        String s = BasicHttpVersionFormat.format(HttpVersion.HTTP_1_1);
        assertEquals("HTTP/1.1", s);
    }
    
    public void testHttpVersionFormattingInvalidInput() throws Exception {
        try {
            BasicHttpVersionFormat.format(null, HttpVersion.HTTP_1_1);
            fail("IllegalArgumentException should habe been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            BasicHttpVersionFormat.format(new CharArrayBuffer(10), (HttpVersion) null);
            fail("IllegalArgumentException should habe been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }

}

