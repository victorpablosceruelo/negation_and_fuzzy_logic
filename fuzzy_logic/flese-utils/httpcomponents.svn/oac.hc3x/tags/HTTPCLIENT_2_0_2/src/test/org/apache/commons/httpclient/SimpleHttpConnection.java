/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/SimpleHttpConnection.java,v 1.15.2.1 2004/02/22 18:21:16 olegk Exp $
 * $Revision: 1.15.2.1 $
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Vector;

import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


/**
 * For test-nohost testing purposes only.
 *
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author Michael Becke
 */
class SimpleHttpConnection extends HttpConnection {

    static Log log = LogFactory.getLog("httpclient.test");

    int hits = 0;

    Vector headers = new Vector();
    Vector bodies = new Vector();

    InputStream inputStream;

    ByteArrayOutputStream bodyOutputStream = null;

    public void addResponse(String header) {
        addResponse(header, "");
    }

    public void addResponse(String header, String body) {
        headers.add(header);
        bodies.add(body);
    }

    public SimpleHttpConnection(String header, String body) {
        this();
        headers.add(header);
        bodies.add(body);
    }

    public SimpleHttpConnection() {
        super(null, -1, "localhost", null, 80, Protocol.getProtocol("http"));
    }

    public SimpleHttpConnection(
        String proxyHost,
        int proxyPort,
        String host,
        String virtualHost,
        int port,
        Protocol protocol) {
        super(proxyHost, proxyPort, host, virtualHost, port, protocol);
    }

    public SimpleHttpConnection(String host, int port){
        super(host, port, Protocol.getProtocol("http"));
    }

    public SimpleHttpConnection(String host, int port, String schema){
        super(host, port, Protocol.getProtocol(schema));
    }

    public void assertOpen() throws IllegalStateException {
        if (inputStream == null) {
            throw new IllegalStateException();
        }
    }

    public void assertNotOpen() throws IllegalStateException{
        if (inputStream != null) {
            throw new IllegalStateException();
        }
    }
    
    public boolean isOpen() {
        return inputStream != null;
    }
    
    public void open() throws IOException {
        if (inputStream != null) return;

        try{
            log.debug("hit: " + hits);
            
            // write the header to a byte array
            ByteArrayOutputStream headerOutputStream = new ByteArrayOutputStream();
            OutputStreamWriter writer = new OutputStreamWriter( headerOutputStream );
            writer.write((String) headers.elementAt(hits));
            // terminate the headers
            writer.write("\r\n");
            writer.close();

            byte[] headerContent = headerOutputStream.toByteArray();
            byte[] bodyContent = HttpConstants.getContentBytes((String)bodies.elementAt(hits));

            // combine the header and body content so they can be read from one steam
            byte[] content = new byte[headerContent.length + bodyContent.length];
            System.arraycopy(headerContent, 0, content, 0, headerContent.length);
            System.arraycopy(bodyContent, 0, content, headerContent.length, bodyContent.length);         

            inputStream = new ByteArrayInputStream( content );
            bodyOutputStream = new ByteArrayOutputStream();
            hits++;
        } catch (ArrayIndexOutOfBoundsException aiofbe) {
            throw new IOException("SimpleHttpConnection has been opened more times " +
                    "than it has responses.  You might need to call addResponse().");
        }
    }

    public void close() {
        if (inputStream != null) {
            try { inputStream.close(); } catch(IOException e) {}
            inputStream = null;
        }
        if (bodyOutputStream != null) {
            try { bodyOutputStream.close(); } catch(IOException e) {}
            bodyOutputStream = null;
        }
    }

    public boolean isResponseAvailable() throws IOException {
        assertOpen();
        return inputStream.available() > 0;
    }

    public boolean isResponseAvailable(int timeout) throws IOException {
        return isResponseAvailable();
    }

    public void write(byte[] data)
    throws IOException, IllegalStateException, HttpRecoverableException {
    }

    public void writeLine()
    throws IOException, IllegalStateException, HttpRecoverableException {
    }

    public String readLine()
    throws IOException, IllegalStateException {
        String str = HttpParser.readLine(inputStream);
        log.debug("read: " + str);
        return str;
    }

    public InputStream getResponseInputStream() {
        return inputStream;
    }

    public OutputStream getRequestOutputStream() {
        return bodyOutputStream;
    }

    public void flushRequestOutputStream() throws IOException {
        assertOpen();
    }
}

