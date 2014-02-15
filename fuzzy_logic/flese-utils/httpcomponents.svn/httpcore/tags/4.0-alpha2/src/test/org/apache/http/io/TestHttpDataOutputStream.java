/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha2/src/test/org/apache/http/io/TestHttpDataOutputStream.java $
 * $Revision: 376458 $
 * $Date: 2006-02-09 23:22:06 +0100 (Thu, 09 Feb 2006) $
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

package org.apache.http.io;

import org.apache.http.mockup.HttpDataTransmitterMockup;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Simple tests for {@link HttpDataOutputStream}.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public class TestHttpDataOutputStream extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestHttpDataOutputStream(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestHttpDataOutputStream.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestHttpDataOutputStream.class);
    }

    public void testConstructor() throws Exception {
        HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup();
        new HttpDataOutputStream(transmitter);
        try {
            new HttpDataOutputStream(null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            //expected
        }
    }
    
    public void testBasicWrite() throws Exception {
    	HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup();
        HttpDataOutputStream outstream = new HttpDataOutputStream(transmitter);
        outstream.write(new byte[] {'a', 'b'}, 0, 2);
        outstream.write('c');
        outstream.flush();
        
        byte[] input = transmitter.getData();
        
        assertNotNull(input);
        byte[] expected = new byte[] {'a', 'b', 'c'};
        assertEquals(expected.length, input.length);
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i], input[i]);
        }
    }
    
    public void testClosedCondition() throws Exception {
    	HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup();
        HttpDataOutputStream outstream = new HttpDataOutputStream(transmitter);
        outstream.close();
        outstream.close();
        
        try {
            byte[] tmp = new byte[2];
            outstream.write(tmp, 0, tmp.length);
            fail("IllegalStateException should have been thrown");
        } catch (IllegalStateException e) {
            //expected
        }
        try {
            outstream.write('a');
            fail("IllegalStateException should have been thrown");
        } catch (IllegalStateException e) {
            //expected
        }
        try {
            outstream.flush();
            fail("IllegalStateException should have been thrown");
        } catch (IllegalStateException e) {
            //expected
        }
    }

}
