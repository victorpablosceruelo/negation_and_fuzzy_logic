/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-nio/src/test/java/org/apache/http/impl/nio/codecs/TestIdentityEncoder.java $
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

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.WritableByteChannel;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.http.util.EncodingUtils;

/**
 * Simple tests for {@link IdentityEncoder}.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Id: TestIdentityEncoder.java 503277 2007-02-03 18:22:45Z olegk $
 */
public class TestIdentityEncoder extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestIdentityEncoder(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestIdentityEncoder.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestIdentityEncoder.class);
    }

    private static ByteBuffer wrap(final String s) {
        return ByteBuffer.wrap(EncodingUtils.getAsciiBytes(s));
    }
    
    private static WritableByteChannel newChannel(final ByteArrayOutputStream baos) {
        return Channels.newChannel(baos);
    }
    
    public void testBasicCoding() throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream(); 
        IdentityEncoder encoder = new IdentityEncoder(newChannel(baos));
        encoder.write(wrap("stuff"));
        encoder.complete();
        
        String s = baos.toString("US-ASCII");
        
        assertTrue(encoder.isCompleted());
        assertEquals("stuff", s);
    }
    
    public void testCodingEmptyBuffer() throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream(); 
        IdentityEncoder encoder = new IdentityEncoder(newChannel(baos));
        encoder.write(wrap("stuff"));
        
        ByteBuffer empty = ByteBuffer.allocate(100);
        empty.flip();
        encoder.write(empty);
        encoder.write(null);
        
        encoder.complete();
        
        String s = baos.toString("US-ASCII");
        
        assertTrue(encoder.isCompleted());
        assertEquals("stuff", s);
    }

    public void testCodingCompleted() throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream(); 
        IdentityEncoder encoder = new IdentityEncoder(newChannel(baos));
        encoder.write(wrap("stuff"));
        encoder.complete();

        try {
            encoder.write(wrap("more stuff"));
            fail("IllegalStateException should have been thrown");
        } catch (IllegalStateException ex) {
            // ignore
        }
    }

    public void testInvalidConstructor() {
        try {
            new IdentityEncoder(null);
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            // ignore
        }
    }

}
