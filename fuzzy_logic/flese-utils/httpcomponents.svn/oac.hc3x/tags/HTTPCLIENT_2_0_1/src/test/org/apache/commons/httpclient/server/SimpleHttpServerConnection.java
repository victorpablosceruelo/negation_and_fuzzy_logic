/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/SimpleHttpServerConnection.java,v 1.1.2.5 2004/02/22 18:21:18 olegk Exp $
 * $Revision: 1.1.2.5 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
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
