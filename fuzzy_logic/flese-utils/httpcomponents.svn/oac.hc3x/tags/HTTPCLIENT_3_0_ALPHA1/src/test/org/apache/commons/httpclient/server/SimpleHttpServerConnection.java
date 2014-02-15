/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/SimpleHttpServerConnection.java,v 1.8 2004/05/11 20:43:55 olegk Exp $
 * $Revision: 1.8 $
 * $Date: 2004-05-11 22:43:55 +0200 (Tue, 11 May 2004) $
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
    private static final String HTTP_ELEMENT_CHARSET = "US-ASCII";

    private SimpleHttpServer server;
    private Socket socket;
    private InputStream in;
    private OutputStream out;

    private int requestNo = 0;

    private boolean keepAlive = false;

    public SimpleHttpServerConnection(SimpleHttpServer server, Socket socket) throws IOException {
        this.server = server;
        this.socket = socket;
        this.in = socket.getInputStream();
        this.out = socket.getOutputStream();
    }

    public void destroy() {
        try {
            if (socket != null) {
                in.close();
                out.close();
                socket.close();
                socket = null;
            }
        } catch (IOException e) {
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
            } while (keepAlive);
        } catch (IOException e) {
            LOG.error("I/O error: " + e.getMessage());
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
     * (must be re-issued for every request if permanent keep-alive is
     * desired).
     *  
     */
    public void connectionKeepAlive() {
        keepAlive = true;
    }

    /**
     * Returns the ResponseWriter used to write the output to the socket.
     * 
     * @return This connection's ResponseWriter
     */
    public ResponseWriter getWriter() {
        try {
            return new ResponseWriter(out);
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e.toString());
        }
    }

    /**
     * Returns the number of requests processed (including the current one) for
     * this connection.
     * 
     * @return
     */
    public int getRequestNumber() {
        return requestNo;
    }

    private void readRequest() throws IOException {
        String line;
        do {
            line = HttpParser.readLine(in, HTTP_ELEMENT_CHARSET);
        } while (line != null && line.length() == 0);

        if (line == null) {
            connectionClose();
            return;
        }

        SimpleRequest request = null;
        try {
            request = new SimpleRequest( 
                RequestLine.parseLine(line),
                HttpParser.parseHeaders(in, HTTP_ELEMENT_CHARSET),
                null);
        } catch (IOException e) {
            connectionClose();
            SimpleResponse response = ErrorResponse.getInstance().
                getResponse(HttpStatus.SC_BAD_REQUEST);
            server.writeResponse(this, response);
            return;
        }
        server.processRequest(this, request);
        out.flush();
    }

    public InputStream getInputStream() {
        return in;
    }

    public OutputStream getOutputStream() {
        return out;
    }

    public boolean isKeepAlive() {
        return this.keepAlive;
    }

}
    