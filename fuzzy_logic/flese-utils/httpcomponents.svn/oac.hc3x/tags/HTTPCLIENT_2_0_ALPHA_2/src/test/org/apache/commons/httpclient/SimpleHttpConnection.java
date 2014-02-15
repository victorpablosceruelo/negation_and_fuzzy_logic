/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/SimpleHttpConnection.java,v 1.7 2003/01/25 12:52:07 olegk Exp $
 * $Revision: 1.7 $
 * $Date: 2003-01-25 13:52:07 +0100 (Sat, 25 Jan 2003) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Tomcat", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
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

import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.util.Vector;


/**
 * For test-nohost testing purposes only.
 *
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 */
class SimpleHttpConnection extends HttpConnection {

    static Log log = LogFactory.getLog("httpclient.test");

    int hits = 0;

    Vector headers = new Vector();
    Vector bodies = new Vector();
    BufferedReader headerReader = null;
    ByteArrayInputStream bodyInputStream = null;
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
        super(null, -1, "localhost", 80, Protocol.getProtocol("http"));
    }

    public SimpleHttpConnection(String host, int port){
        super(host, port, Protocol.getProtocol("http"));
    }

    public SimpleHttpConnection(String host, int port, String schema){
        super(host, port, Protocol.getProtocol(schema));
    }

    public void assertOpen() throws IllegalStateException {
        if (bodyInputStream == null) {
            throw new IllegalStateException();
        }
    }

    public void assertNotOpen() throws IllegalStateException{
        if (bodyInputStream != null) {
            throw new IllegalStateException();
        }
    }
    
    public void open() throws IOException {
        if (headerReader != null) return;

        try{
            log.debug("hit: " + hits);
            headerReader = new BufferedReader(
                    new StringReader((String)headers.elementAt(hits)));
            bodyInputStream = new ByteArrayInputStream(
              HttpConstants.getContentBytes((String)bodies.elementAt(hits)));
            bodyOutputStream = new ByteArrayOutputStream();
            hits++;
        } catch (ArrayIndexOutOfBoundsException aiofbe) {
            throw new IOException("SimpleHttpConnection has been opened more times " +
                    "than it has responses.  You might need to call addResponse().");
        }
    }

    public void close() {
        if (headerReader != null) {
            try { headerReader.close(); } catch(IOException e) {}
            headerReader = null;
        }
        if (bodyInputStream != null) {
            try { bodyInputStream.close(); } catch(IOException e) {}
            bodyInputStream = null;
        }
        if (bodyOutputStream != null) {
            try { bodyOutputStream.close(); } catch(IOException e) {}
            bodyOutputStream = null;
        }
    }

    public void write(byte[] data)
    throws IOException, IllegalStateException, HttpRecoverableException {
    }

    public void writeLine()
    throws IOException, IllegalStateException, HttpRecoverableException {
    }

    public String readLine()
    throws IOException, IllegalStateException {
        String str = headerReader.readLine();
        log.debug("read: " + str);
        return str;
    }

    
    public InputStream getResponseInputStream() {
        return bodyInputStream;
    }

    public OutputStream getRequestOutputStream() {
        return bodyOutputStream;
    }

}

