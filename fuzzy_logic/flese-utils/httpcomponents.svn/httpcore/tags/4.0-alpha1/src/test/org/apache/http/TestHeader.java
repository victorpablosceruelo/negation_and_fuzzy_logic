/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha1/src/test/org/apache/http/TestHeader.java $
 * $Revision: 379761 $
 * $Date: 2006-02-22 14:01:42 +0100 (Wed, 22 Feb 2006) $
 * 
 * ====================================================================
 *
 *  Copyright 1999-2006 The Apache Software Foundation
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
 */

package org.apache.http;

import org.apache.http.io.CharArrayBuffer;
import org.apache.http.io.HttpDataReceiver;
import org.apache.http.mockup.HttpDataReceiverMockup;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit tests for {@link Header}.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public class TestHeader extends TestCase {

    public TestHeader(String testName) {
        super(testName);
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestHeader.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    public static Test suite() {
        return new TestSuite(TestHeader.class);
    }

    public void testBasicConstructor() {
        Header header = new Header("name", "value");
        assertEquals("name", header.getName()); 
        assertEquals("value", header.getValue()); 
    }
    
    public void testBasicConstructorNullValue() {
        Header header = new Header("name", null);
        assertEquals("name", header.getName()); 
        assertEquals(null, header.getValue()); 
    }
    
    public void testInvalidName() {
        try {
            new Header(null, null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            //expected
        }
    }
    
    public void testToString() {
        Header header1 = new Header("name1", "value1");
        assertEquals("name1: value1", header1.toString());
        Header header2 = new Header("name2", null);
        assertEquals("name2: ", header2.toString());
    }
    
    public void testHeaderElements() {
        Header header = new Header("name", "element1 = value1, element2; param1 = value1, element3");
        HeaderElement[] elements = header.getElements(); 
        assertNotNull(elements); 
        assertEquals(3, elements.length); 
        assertEquals("element1", elements[0].getName()); 
        assertEquals("value1", elements[0].getValue()); 
        assertEquals("element2", elements[1].getName()); 
        assertEquals(null, elements[1].getValue()); 
        assertEquals("element3", elements[2].getName()); 
        assertEquals(null, elements[2].getValue()); 
        assertEquals(1, elements[1].getParameters().length); 
        
        header = new Header("name", null);
        elements = header.getElements();
        assertNotNull(elements); 
        assertEquals(0, elements.length); 
    }    
        
    public void testBasicHeaderParsing() throws Exception {
        String s = 
            "header1: stuff\r\n" + 
            "header2  : stuff \r\n" + 
            "header3: stuff\r\n" + 
            "     and more stuff\r\n" + 
            "\t and even more stuff\r\n" +  
            "     \r\n" +  
            "\r\n"; 
        HttpDataReceiver receiver = new HttpDataReceiverMockup(s, "US-ASCII"); 
        Header[] headers = Header.parseAll(receiver);
        assertNotNull(headers);
        assertEquals(3, headers.length);
        assertEquals("header1", headers[0].getName());
        assertEquals("stuff", headers[0].getValue());
        assertEquals("header2", headers[1].getName());
        assertEquals("stuff", headers[1].getValue());
        assertEquals("header3", headers[2].getName());
        assertEquals("stuff and more stuff and even more stuff", headers[2].getValue());
    }

    public void testBufferedHeader() throws Exception {
        String s = 
            "header1  : stuff; param1 = value1; param2 = \"value 2\" \r\n" + 
            "\r\n"; 
        HttpDataReceiver receiver = new HttpDataReceiverMockup(s, "US-ASCII"); 
        Header[] headers = Header.parseAll(receiver);
        assertNotNull(headers);
        assertEquals(1, headers.length);
        assertEquals("header1  : stuff; param1 = value1; param2 = \"value 2\" ", headers[0].toString());
        HeaderElement[] elements = headers[0].getElements();
        assertNotNull(elements);
        assertEquals(1, elements.length);
        assertEquals("stuff", elements[0].getName());
        assertEquals(null, elements[0].getValue());
        NameValuePair[] params = elements[0].getParameters();
        assertNotNull(params);
        assertEquals(2, params.length);
        assertEquals("param1", params[0].getName());
        assertEquals("value1", params[0].getValue());
        assertEquals("param2", params[1].getName());
        assertEquals("value 2", params[1].getValue());
    }

    public void testParsingInvalidHeaders() throws Exception {
        String s = "    stuff\r\n" + 
            "header1: stuff\r\n" + 
            "\r\n"; 
        HttpDataReceiver receiver = new HttpDataReceiverMockup(s, "US-ASCII");
        try {
            Header.parseAll(receiver);
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException ex) {
            // expected
        }
        s = "  :  stuff\r\n" + 
            "header1: stuff\r\n" + 
            "\r\n"; 
        receiver = new HttpDataReceiverMockup(s, "US-ASCII");
        try {
            Header.parseAll(receiver);
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException ex) {
            // expected
        }
    }
    
    public void testParsingMalformedFirstHeader() throws Exception {
        String s = 
            "    header1: stuff\r\n" + 
            "header2  : stuff \r\n"; 
        HttpDataReceiver receiver = new HttpDataReceiverMockup(s, "US-ASCII"); 
        Header[] headers = Header.parseAll(receiver);
        assertNotNull(headers);
        assertEquals(2, headers.length);
        assertEquals("header1", headers[0].getName());
        assertEquals("stuff", headers[0].getValue());
        assertEquals("header2", headers[1].getName());
        assertEquals("stuff", headers[1].getValue());
    }
    
    public void testEmptyDataStream() throws Exception {
        String s = ""; 
        HttpDataReceiver receiver = new HttpDataReceiverMockup(s, "US-ASCII"); 
        Header[] headers = Header.parseAll(receiver);
        assertNotNull(headers);
        assertEquals(0, headers.length);
    }
    
    public void testHeaderFormatting() throws Exception {
        Header header1 = new Header("name", "value");
        String s = Header.format(header1); 
        assertEquals("name: value", s);
        Header header2 = new Header("name", null);
        s = Header.format(header2); 
        assertEquals("name: ", s);
    }
    
    public void testHeaderFormattingInvalidInput() throws Exception {
        try {
            Header.format(null, new Header("name", "value"));
            fail("IllegalArgumentException should habe been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            Header.format(new CharArrayBuffer(10), (Header) null);
            fail("IllegalArgumentException should habe been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }
    
}

