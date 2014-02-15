/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestHeaderElement.java,v 1.7 2004/02/22 18:08:49 olegk Exp $
 * $Revision: 571954 $
 * $Date: 2007-09-02 13:05:21 +0200 (Sun, 02 Sep 2007) $
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.http.message;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.http.HeaderElement;
import org.apache.http.NameValuePair;
import org.apache.http.util.CharArrayBuffer;

/**
 * Simple tests for {@link HeaderElement}.
 *
 * @author Rodney Waldhoff
 * @author <a href="mailto:bcholmes@interlog.com">B.C. Holmes</a>
 * @author <a href="mailto:jericho@thinkfree.com">Park, Sung-Gu</a>
 * @author <a href="mailto:oleg at ural.ru">oleg Kalnichevski</a>
 * @version $Id: TestHeaderElement.java 571954 2007-09-02 11:05:21Z olegk $
 */
public class TestHeaderElement extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestHeaderElement(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestHeaderElement.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestHeaderElement.class);
    }

    public void testConstructor3() throws Exception {
        HeaderElement element = new BasicHeaderElement("name", "value", 
                new NameValuePair[] {
                    new BasicNameValuePair("param1", "value1"),
                    new BasicNameValuePair("param2", "value2")
                } );
        assertEquals("name", element.getName());
        assertEquals("value", element.getValue());
        assertEquals(2, element.getParameters().length);
        assertEquals("value1", element.getParameterByName("param1").getValue());
        assertEquals("value2", element.getParameterByName("param2").getValue());
    }

    public void testConstructor2() throws Exception {
        HeaderElement element = new BasicHeaderElement("name", "value");
        assertEquals("name", element.getName());
        assertEquals("value", element.getValue());
        assertEquals(0, element.getParameters().length);
    }


    public void testConstructor1() throws Exception {
        String s = "name = value; param1 = value1";
        NameValuePair[] nvps = BasicHeaderValueParser.parseParameters(s, null);
        HeaderElement element = new BasicHeaderElement(nvps);
        assertEquals("name", element.getName());
        assertEquals("value", element.getValue());
        assertEquals(1, element.getParameters().length);
        assertEquals("value1", element.getParameterByName("param1").getValue());
    }
    
    public void testInvalidName() {
        try {
            new BasicHeaderElement(null, null, null); 
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            //expected
        }
    }

    public void testParamByName() throws Exception {
        CharArrayBuffer buffer = new CharArrayBuffer(64);
        buffer.append("name = value; param1 = value1; param2 = value2");
        HeaderElement element = BasicHeaderValueParser.DEFAULT
            .parseHeaderElement(buffer, 0, buffer.length()); 
        assertEquals("value1", element.getParameterByName("param1").getValue());
        assertEquals("value2", element.getParameterByName("param2").getValue());
        assertNull(element.getParameterByName("param3"));
        try {
            element.getParameterByName(null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            //expected
        }
    }

    public void testHashCode() {
        HeaderElement element1 = new BasicHeaderElement("name", "value", 
                new NameValuePair[] {
                    new BasicNameValuePair("param1", "value1"),
                    new BasicNameValuePair("param2", "value2")
                } );
        HeaderElement element2 = new BasicHeaderElement("name", "value", 
                new NameValuePair[] {
                    new BasicNameValuePair("param2", "value2"),
                    new BasicNameValuePair("param1", "value1")
                } );
        HeaderElement element3 = new BasicHeaderElement("name", "value"); 
        HeaderElement element4 = new BasicHeaderElement("name", "value"); 
        HeaderElement element5 = new BasicHeaderElement("name", "value", 
                new NameValuePair[] {
                    new BasicNameValuePair("param1", "value1"),
                    new BasicNameValuePair("param2", "value2")
                } );
        assertTrue(element1.hashCode() != element2.hashCode());
        assertTrue(element1.hashCode() != element3.hashCode());
        assertTrue(element2.hashCode() != element3.hashCode());
        assertTrue(element3.hashCode() == element4.hashCode());
        assertTrue(element1.hashCode() == element5.hashCode());
    }
    
    public void testEquals() {
        HeaderElement element1 = new BasicHeaderElement("name", "value", 
                new NameValuePair[] {
                    new BasicNameValuePair("param1", "value1"),
                    new BasicNameValuePair("param2", "value2")
                } );
        HeaderElement element2 = new BasicHeaderElement("name", "value", 
                new NameValuePair[] {
                    new BasicNameValuePair("param2", "value2"),
                    new BasicNameValuePair("param1", "value1")
                } );
        HeaderElement element3 = new BasicHeaderElement("name", "value"); 
        HeaderElement element4 = new BasicHeaderElement("name", "value"); 
        HeaderElement element5 = new BasicHeaderElement("name", "value", 
                new NameValuePair[] {
                    new BasicNameValuePair("param1", "value1"),
                    new BasicNameValuePair("param2", "value2")
                } );
        assertTrue(element1.equals(element1));
        assertTrue(!element1.equals(element2));
        assertTrue(!element1.equals(element3));
        assertTrue(!element2.equals(element3));
        assertTrue(element3.equals(element4));
        assertTrue(element1.equals(element5));
        assertFalse(element1.equals(null));
        assertFalse(element1.equals("name = value; param1 = value1; param2 = value2"));
    }
    
    public void testToString() {
        CharArrayBuffer buffer = new CharArrayBuffer(64);
        String s = "name=value; param1=value1; param2=value2";
        buffer.append(s);


        HeaderElement element = BasicHeaderValueParser.DEFAULT
            .parseHeaderElement(buffer, 0, buffer.length());
        assertEquals(s, element.toString());

        s = "name; param1=value1; param2=value2";
        buffer.clear();
        buffer.append(s);

        element = BasicHeaderValueParser.DEFAULT
            .parseHeaderElement(buffer, 0, buffer.length());
        assertEquals(s, element.toString());
    }
}
