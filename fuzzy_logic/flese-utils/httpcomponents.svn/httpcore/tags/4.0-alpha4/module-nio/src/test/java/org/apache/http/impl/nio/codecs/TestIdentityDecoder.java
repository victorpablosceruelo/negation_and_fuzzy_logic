/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-nio/src/test/java/org/apache/http/impl/nio/codecs/TestIdentityDecoder.java $
 * $Revision: 503277 $
 * $Date: 2007-02-03 19:22:45 +0100 (Sat, 03 Feb 2007) $
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

package org.apache.http.impl.nio.codecs;

import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.http.impl.nio.reactor.SessionInputBuffer;
import org.apache.http.nio.mockup.ReadableByteChannelMockup;

/**
 * Simple tests for {@link LengthDelimitedDecoder}.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Id: TestIdentityDecoder.java 503277 2007-02-03 18:22:45Z olegk $
 */
public class TestIdentityDecoder extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestIdentityDecoder(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestIdentityDecoder.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestIdentityDecoder.class);
    }

    private static String convert(final ByteBuffer src) {
        src.flip();
        StringBuffer buffer = new StringBuffer(src.remaining()); 
        while (src.hasRemaining()) {
            buffer.append((char)(src.get() & 0xff));
        }
        return buffer.toString();
    }

    public void testBasicDecoding() throws Exception {
        ReadableByteChannel channel = new ReadableByteChannelMockup(
                new String[] {"stuff;", "more stuff"}, "US-ASCII"); 
        
        SessionInputBuffer inbuf = new SessionInputBuffer(1024, 256); 
        IdentityDecoder decoder = new IdentityDecoder(channel, inbuf); 
        
        ByteBuffer dst = ByteBuffer.allocate(1024); 
        
        int bytesRead = decoder.read(dst);
        assertEquals(6, bytesRead);
        assertEquals("stuff;", convert(dst));
        assertFalse(decoder.isCompleted());
        
        dst.clear();
        bytesRead = decoder.read(dst);
        assertEquals(10, bytesRead);
        assertEquals("more stuff", convert(dst));
        assertFalse(decoder.isCompleted());
        
        dst.clear();
        bytesRead = decoder.read(dst);
        assertEquals(-1, bytesRead);
        assertTrue(decoder.isCompleted());

        dst.clear();
        bytesRead = decoder.read(dst);
        assertEquals(-1, bytesRead);
        assertTrue(decoder.isCompleted());
    }
    
    public void testDecodingFromSessionBuffer() throws Exception {
        ReadableByteChannel channel = new ReadableByteChannelMockup(
                new String[] {"stuff;", "more stuff"}, "US-ASCII"); 
        
        SessionInputBuffer inbuf = new SessionInputBuffer(1024, 256);
        inbuf.fill(channel);
        
        assertEquals(6, inbuf.length());
        
        IdentityDecoder decoder = new IdentityDecoder(channel, inbuf); 
        
        ByteBuffer dst = ByteBuffer.allocate(1024); 
        
        int bytesRead = decoder.read(dst);
        assertEquals(6, bytesRead);
        assertEquals("stuff;", convert(dst));
        assertFalse(decoder.isCompleted());
        
        dst.clear();
        bytesRead = decoder.read(dst);
        assertEquals(10, bytesRead);
        assertEquals("more stuff", convert(dst));
        assertFalse(decoder.isCompleted());
        
        dst.clear();
        bytesRead = decoder.read(dst);
        assertEquals(-1, bytesRead);
        assertTrue(decoder.isCompleted());

        dst.clear();
        bytesRead = decoder.read(dst);
        assertEquals(-1, bytesRead);
        assertTrue(decoder.isCompleted());
    }

    public void testInvalidInput() throws Exception {
        String s = "stuff";
        ReadableByteChannel channel = new ReadableByteChannelMockup(
                new String[] {s}, "US-ASCII"); 
    
        SessionInputBuffer inbuf = new SessionInputBuffer(1024, 256); 
        IdentityDecoder decoder = new IdentityDecoder(channel, inbuf);
        
        try {
            decoder.read(null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }
    
}
