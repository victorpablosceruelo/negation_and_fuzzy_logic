/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-main/src/test/java/org/apache/http/impl/TestHttpDataReceiverAndTransmitter.java $
 * $Revision: 549544 $
 * $Date: 2007-06-21 18:51:27 +0200 (Thu, 21 Jun 2007) $
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

package org.apache.http.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.apache.http.io.HttpTransportMetrics;

import org.apache.http.params.BasicHttpParams;
import org.apache.http.mockup.HttpDataReceiverMockup;
import org.apache.http.mockup.HttpDataTransmitterMockup;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.CharArrayBuffer;

public class TestHttpDataReceiverAndTransmitter extends TestCase {

    public TestHttpDataReceiverAndTransmitter(String testName) {
        super(testName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestHttpDataReceiverAndTransmitter.class);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestHttpDataReceiverAndTransmitter.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    public void testInit() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        new HttpDataTransmitterMockup(out); 
        try {
            new HttpDataTransmitterMockup(null, new BasicHttpParams()); 
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            //expected
        }
        ByteArrayInputStream in = new ByteArrayInputStream(out.toByteArray());
        new HttpDataReceiverMockup(in, 10); 
        try {
            new HttpDataReceiverMockup(in, -10); 
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            //expected
        }
        try {
            new HttpDataTransmitterMockup(out, -10); 
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            //expected
        }
        try {
            new HttpDataReceiverMockup((InputStream)null, 1024); 
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            //expected
        }
    }
    
    public void testBasicReadWriteLine() throws Exception {
        
        String[] teststrs = new String[5];
        teststrs[0] = "Hello";
        teststrs[1] = "This string should be much longer than the size of the output buffer " +
                "which is only 16 bytes for this test";
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < 15; i++) {
            buffer.append("123456789 ");
        }
        buffer.append("and stuff like that");
        teststrs[2] = buffer.toString();
        teststrs[3] = "";
        teststrs[4] = "And goodbye";
        
        CharArrayBuffer chbuffer = new CharArrayBuffer(16); 
        HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup(); 
        for (int i = 0; i < teststrs.length; i++) {
            chbuffer.clear();
            chbuffer.append(teststrs[i]);
            transmitter.writeLine(chbuffer);
        }
        //these write operations should have no effect
        transmitter.writeLine((String)null);
        transmitter.writeLine((CharArrayBuffer)null);
        transmitter.flush();
        
        HttpTransportMetrics tmetrics = transmitter.getMetrics();
        long writedBytes = tmetrics.getBytesTransferred();
        long expWrited = 0;
        for (int i = 0; i < teststrs.length; i++) {
            expWrited += (teststrs[i].length() + 2/*CRLF*/);
        }
        assertEquals(expWrited, writedBytes);
        
        HttpDataReceiverMockup receiver = new HttpDataReceiverMockup(
        		transmitter.getData());

        for (int i = 0; i < teststrs.length; i++) {
            assertEquals(teststrs[i], receiver.readLine());
        }
        
        assertNull(receiver.readLine());
        assertNull(receiver.readLine());
        tmetrics = receiver.getMetrics();
        long readedBytes = tmetrics.getBytesTransferred();
        assertEquals(expWrited, readedBytes);
    }

    public void testComplexReadWriteLine() throws Exception {
        HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup(); 
        transmitter.write(new byte[] {'a', '\n'});
        transmitter.write(new byte[] {'\r', '\n'});
        transmitter.write(new byte[] {'\r', '\r', '\n'});
        transmitter.write(new byte[] {'\n'});
        //these write operations should have no effect
        transmitter.write(null);
        transmitter.write(null, 0, 12);
        
        transmitter.flush();

        long writedBytes = transmitter.getMetrics().getBytesTransferred();
        assertEquals(8, writedBytes);
        
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < 14; i++) {
            buffer.append("a");
        }
        String s1 = buffer.toString();
        buffer.append("\r\n");
        transmitter.write(buffer.toString().getBytes("US-ASCII"));
        transmitter.flush();
        writedBytes = transmitter.getMetrics().getBytesTransferred();
        assertEquals(8 + 14 +2, writedBytes);

        buffer.setLength(0);
        for (int i = 0; i < 15; i++) {
            buffer.append("a");
        }
        String s2 = buffer.toString();
        buffer.append("\r\n");
        transmitter.write(buffer.toString().getBytes("US-ASCII"));
        transmitter.flush();
        writedBytes = transmitter.getMetrics().getBytesTransferred();
        assertEquals(8 + 14 + 2 + 15 + 2 , writedBytes);

        buffer.setLength(0);
        for (int i = 0; i < 16; i++) {
            buffer.append("a");
        }
        String s3 = buffer.toString();
        buffer.append("\r\n");
        transmitter.write(buffer.toString().getBytes("US-ASCII"));
        transmitter.flush();
        writedBytes = transmitter.getMetrics().getBytesTransferred();
        assertEquals(8 + 14 + 2 + 15 + 2 + 16 + 2, writedBytes);

        transmitter.write(new byte[] {'a'});
        transmitter.flush();
        writedBytes = transmitter.getMetrics().getBytesTransferred();
        assertEquals(8 + 14 + 2 + 15 + 2 + 16 + 2 + 1, writedBytes);
        
        HttpDataReceiverMockup receiver = new HttpDataReceiverMockup(
        		transmitter.getData());

        assertEquals("a", receiver.readLine());
        assertEquals("", receiver.readLine());
        assertEquals("\r", receiver.readLine());
        assertEquals("", receiver.readLine());
        assertEquals(s1, receiver.readLine());
        assertEquals(s2, receiver.readLine());
        assertEquals(s3, receiver.readLine());
        assertEquals("a", receiver.readLine());
        assertNull(receiver.readLine());
        assertNull(receiver.readLine());
        long received = receiver.getMetrics().getBytesTransferred();
        assertEquals(writedBytes, received);
    }
    
    public void testBasicReadWriteLineLargeBuffer() throws Exception {
        
        String[] teststrs = new String[5];
        teststrs[0] = "Hello";
        teststrs[1] = "This string should be much longer than the size of the output buffer " +
                "which is only 16 bytes for this test";
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < 15; i++) {
            buffer.append("123456789 ");
        }
        buffer.append("and stuff like that");
        teststrs[2] = buffer.toString();
        teststrs[3] = "";
        teststrs[4] = "And goodbye";
        
        CharArrayBuffer chbuffer = new CharArrayBuffer(16); 
        HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup(); 
        for (int i = 0; i < teststrs.length; i++) {
            chbuffer.clear();
            chbuffer.append(teststrs[i]);
            transmitter.writeLine(chbuffer);
        }
        //these write operations should have no effect
        transmitter.writeLine((String)null);
        transmitter.writeLine((CharArrayBuffer)null);
        transmitter.flush();
        
        long writedBytes = transmitter.getMetrics().getBytesTransferred();
        long expWrited = 0;
        for (int i = 0; i < teststrs.length; i++) {
            expWrited += (teststrs[i].length() + 2/*CRLF*/);
        }
        assertEquals(expWrited, writedBytes);
        
        HttpDataReceiverMockup receiver = new HttpDataReceiverMockup(
        		transmitter.getData(), 1024);

        for (int i = 0; i < teststrs.length; i++) {
            assertEquals(teststrs[i], receiver.readLine());
        }
        assertNull(receiver.readLine());
        assertNull(receiver.readLine());
        long readedBytes = receiver.getMetrics().getBytesTransferred();
        assertEquals(expWrited, readedBytes);
    }

    public void testReadWriteBytes() throws Exception {
        // make the buffer larger than that of transmitter
        byte[] out = new byte[40];
        for (int i = 0; i < out.length; i++) {
            out[i] = (byte)('0' + i);
        }
        HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup();
        int off = 0;
        int remaining = out.length;
        while (remaining > 0) {
            int chunk = 10;
            if (chunk > remaining) {
                chunk = remaining;
            }
            transmitter.write(out, off, chunk);
            off += chunk;
            remaining -= chunk;
        }
        transmitter.flush();
        long writedBytes = transmitter.getMetrics().getBytesTransferred();
        assertEquals(out.length, writedBytes);

        byte[] tmp = transmitter.getData();
        assertEquals(out.length, tmp.length);
        for (int i = 0; i < out.length; i++) {
            assertEquals(out[i], tmp[i]);
        }
        
        HttpDataReceiverMockup receiver = new HttpDataReceiverMockup(tmp);

        // these read operations will have no effect
        assertEquals(0, receiver.read(null, 0, 10));
        assertEquals(0, receiver.read(null));        
        long receivedBytes = receiver.getMetrics().getBytesTransferred();
        assertEquals(0, receivedBytes);
        
        byte[] in = new byte[40];
        off = 0;
        remaining = in.length;
        while (remaining > 0) {
            int chunk = 10;
            if (chunk > remaining) {
                chunk = remaining;
            }
            int l = receiver.read(in, off, chunk);
            if (l == -1) {
                break;
            }
            off += l;
            remaining -= l;
        }
        for (int i = 0; i < out.length; i++) {
            assertEquals(out[i], in[i]);
        }
        assertEquals(-1, receiver.read(tmp));
        assertEquals(-1, receiver.read(tmp));
        receivedBytes = receiver.getMetrics().getBytesTransferred();
        assertEquals(out.length, receivedBytes);
    }
    
    public void testReadWriteByte() throws Exception {
        // make the buffer larger than that of transmitter
        byte[] out = new byte[40];
        for (int i = 0; i < out.length; i++) {
            out[i] = (byte)(120 + i);
        }
        HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup();
        for (int i = 0; i < out.length; i++) {
            transmitter.write(out[i]);
        }
        transmitter.flush();
        long writedBytes = transmitter.getMetrics().getBytesTransferred();
        assertEquals(out.length, writedBytes);

        byte[] tmp = transmitter.getData();
        assertEquals(out.length, tmp.length);
        for (int i = 0; i < out.length; i++) {
            assertEquals(out[i], tmp[i]);
        }
        
        HttpDataReceiverMockup receiver = new HttpDataReceiverMockup(tmp);
        byte[] in = new byte[40];
        for (int i = 0; i < in.length; i++) {
            in[i] = (byte)receiver.read();
        }
        for (int i = 0; i < out.length; i++) {
            assertEquals(out[i], in[i]);
        }
        assertEquals(-1, receiver.read());
        assertEquals(-1, receiver.read());
        long readedBytes = receiver.getMetrics().getBytesTransferred();
        assertEquals(out.length, readedBytes);
    }

    public void testLineLimit() throws Exception {
        HttpParams params = new BasicHttpParams();
        String s = "a very looooooooooooooooooooooooooooooooooooooong line\r\n     ";
        byte[] tmp = s.getBytes("US-ASCII"); 
        // no limit
        params.setIntParameter(HttpConnectionParams.MAX_LINE_LENGTH, 0);
        HttpDataReceiverMockup receiver1 = new HttpDataReceiverMockup(tmp, 5, params);
        assertNotNull(receiver1.readLine());
        long readedBytes = receiver1.getMetrics().getBytesTransferred();
        assertEquals(60, readedBytes);
        
        // 15 char limit
        params.setIntParameter(HttpConnectionParams.MAX_LINE_LENGTH, 15);
        HttpDataReceiverMockup receiver2 = new HttpDataReceiverMockup(tmp, 5, params);
        try {
            receiver2.readLine();
            fail("IOException should have been thrown");
        } catch (IOException ex) {
            // expected
            readedBytes = receiver2.getMetrics().getBytesTransferred();
            assertEquals(20, readedBytes);
        }
    }

    static final int SWISS_GERMAN_HELLO [] = {
        0x47, 0x72, 0xFC, 0x65, 0x7A, 0x69, 0x5F, 0x7A, 0xE4, 0x6D, 0xE4
    };
        
    static final int RUSSIAN_HELLO [] = {
        0x412, 0x441, 0x435, 0x43C, 0x5F, 0x43F, 0x440, 0x438, 
        0x432, 0x435, 0x442 
    }; 
    
    private static String constructString(int [] unicodeChars) {
        StringBuffer buffer = new StringBuffer();
        if (unicodeChars != null) {
            for (int i = 0; i < unicodeChars.length; i++) {
                buffer.append((char)unicodeChars[i]); 
            }
        }
        return buffer.toString();
    }

    public void testMultibyteCodedReadWriteLine() throws Exception {
        String s1 = constructString(SWISS_GERMAN_HELLO);
        String s2 = constructString(RUSSIAN_HELLO);
        String s3 = "Like hello and stuff";
        
        HttpParams params = new BasicHttpParams(null);
        HttpProtocolParams.setHttpElementCharset(params, "UTF-8");
        
        HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup(params);

        CharArrayBuffer chbuffer = new CharArrayBuffer(16); 
        for (int i = 0; i < 10; i++) {
            chbuffer.clear();
            chbuffer.append(s1);
            transmitter.writeLine(chbuffer);
            chbuffer.clear();
            chbuffer.append(s2);
            transmitter.writeLine(chbuffer);
            chbuffer.clear();
            chbuffer.append(s3);
            transmitter.writeLine(chbuffer);
        }
        transmitter.flush();
        long writedBytes = transmitter.getMetrics().getBytesTransferred();
        long expBytes = ((s1.toString().getBytes("UTF-8").length + 2)+
                (s2.toString().getBytes("UTF-8").length + 2) +
                (s3.toString().getBytes("UTF-8").length + 2)) * 10;
        assertEquals(expBytes, writedBytes);
        
        HttpDataReceiverMockup receiver = new HttpDataReceiverMockup(
        		transmitter.getData(), params);

        for (int i = 0; i < 10; i++) {
            assertEquals(s1, receiver.readLine());
            assertEquals(s2, receiver.readLine());
            assertEquals(s3, receiver.readLine());
        }
        assertNull(receiver.readLine());
        assertNull(receiver.readLine());
        long readedBytes = receiver.getMetrics().getBytesTransferred();
        assertEquals(expBytes, readedBytes);
    }

    public void testNonAsciiReadWriteLine() throws Exception {
        String s1 = constructString(SWISS_GERMAN_HELLO);
        
        HttpParams params = new BasicHttpParams(null);
        HttpProtocolParams.setHttpElementCharset(params, HTTP.ISO_8859_1);
        
        HttpDataTransmitterMockup transmitter = new HttpDataTransmitterMockup(params);

        CharArrayBuffer chbuffer = new CharArrayBuffer(16); 
        for (int i = 0; i < 10; i++) {
            chbuffer.clear();
            chbuffer.append(s1);
            transmitter.writeLine(chbuffer);
        }
        transmitter.flush();
        long writedBytes = transmitter.getMetrics().getBytesTransferred();
        long expBytes = ((s1.toString().getBytes(HTTP.ISO_8859_1).length + 2)) * 10;
        assertEquals(expBytes, writedBytes);
        
        HttpDataReceiverMockup receiver = new HttpDataReceiverMockup(
                transmitter.getData(),
                params);
        HttpProtocolParams.setHttpElementCharset(params, HTTP.ISO_8859_1);

        for (int i = 0; i < 10; i++) {
            assertEquals(s1, receiver.readLine());
        }
        assertNull(receiver.readLine());
        assertNull(receiver.readLine());
        long readedBytes = receiver.getMetrics().getBytesTransferred();
        assertEquals(expBytes, readedBytes);
    }

    public void testInvalidCharArrayBuffer() throws Exception {
        HttpDataReceiverMockup receiver = new HttpDataReceiverMockup(new byte[] {});
        try {
            receiver.readLine(null); 
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException ex) {
            //expected
            long readedBytes = receiver.getMetrics().getBytesTransferred();
            assertEquals(0, readedBytes);
        }
    }
    
}

