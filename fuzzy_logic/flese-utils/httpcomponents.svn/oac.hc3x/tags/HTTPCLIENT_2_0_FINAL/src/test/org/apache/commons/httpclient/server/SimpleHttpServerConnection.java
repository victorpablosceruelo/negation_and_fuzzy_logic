/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/SimpleHttpServerConnection.java,v 1.1.2.4 2003/11/24 23:16:42 oglueck Exp $
 * $Revision: 1.1.2.4 $
 * $Date: 2003-11-25 00:16:42 +0100 (Tue, 25 Nov 2003) $
 *
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
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
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

package org.apache.commons.httpclient.server;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.net.SocketException;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpParser;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * A connection to the SimpleHttpServer. 
 * 
 * @author Christian Kohlschuetter
 */
public class SimpleHttpServerConnection implements Runnable {
    
    private static final Log LOG = LogFactory.getLog(SimpleHttpServerConnection.class);
    
    private SimpleHttpServer server;
    private Socket socket;
    private InputStream in;
    private OutputStream out;

    private int requestNo = 0;
    
    private boolean keepAlive = false;

	private RequestLine requestLine;

	private Header[] headers;

    public SimpleHttpServerConnection(SimpleHttpServer server, Socket socket) throws IOException {
        this.server = server;
        this.socket = socket;
        this.in = socket.getInputStream();
        this.out = socket.getOutputStream();
    }

    public void destroy() {
        try {
            if(socket != null) {
                in.close();
                out.close();
                socket.close();
                socket = null;
            } 
        } catch(IOException e) {
            // fail("Unexpected exception: " + e);
        }
        server.removeConnection(this);
    }

    public void run() {
        requestNo = 0;
        try {
            do {
                keepAlive = false;
                
                ++this.requestNo;
                readRequest();
            } while(keepAlive);
        } catch (SocketException ignore) {
        } catch (IOException e) {
            LOG.error("ServerConnection read error", e);
            throw new RuntimeException(e.getMessage());
        } finally {
            destroy();
        }
    }
    
    /**
     * Requests to close connection after processing this request.
     */
    public void connectionClose() {
        keepAlive = false;
    }

    /**
     * Requests to keep the connection alive after processing this request
     * (must be re-issued for every request if permanent keep-alive is desired).
     * 
     */
    public void connectionKeepAlive() {
        keepAlive = true;
    }
    
    /**
     * Returns the ResponseWriter used to write the output to the socket.
     * 
     * @return  This connection's ResponseWriter
     */
    public ResponseWriter getWriter() {
        try {
			return new ResponseWriter(out);
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e.toString());
		}
    }
    
    /**
     * Returns the number of requests processed (including the current one)
     * for this connection.
     * 
     * @return
     */
    public int getRequestNumber() {
        return requestNo;
    }
            
    private void readRequest() throws IOException {
        String line;
        do {
            line = HttpParser.readLine(in);
        } while(line != null && line.length() == 0);
        
        if(line == null) {
            connectionClose();
            return;
        }

        try {
            requestLine = RequestLine.parseLine(line);
            headers = HttpParser.parseHeaders(in);
        } catch(IOException e) {
            connectionClose();
            ErrorResponse.getInstance().getResponse(HttpStatus.SC_BAD_REQUEST).processRequest(this);
            return;     
        }
        server.processRequest(this);
        out.flush();
    }
    
    public Header[] getHeaders() {
    	Header[] copy = new Header[headers.length];
    	System.arraycopy(headers, 0, copy, 0, headers.length);
    	return copy;
    }
    
    public RequestLine getRequestLine() {
    	return requestLine;
    }

	public InputStream getInputStream() {
		return in;
	}

	public OutputStream getOutputStream() {
		return out;
	}
}
