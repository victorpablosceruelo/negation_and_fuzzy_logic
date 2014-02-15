/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha4/module-client/src/main/java/org/apache/http/impl/conn/DefaultClientConnection.java $
 * $Revision: 653041 $
 * $Date: 2008-05-03 12:39:28 +0200 (Sat, 03 May 2008) $
 *
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

package org.apache.http.impl.conn;


import java.io.IOException;
import java.net.Socket;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.Header;
import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpResponseFactory;
import org.apache.http.params.HttpParams;
import org.apache.http.impl.SocketHttpClientConnection;
import org.apache.http.io.HttpMessageParser;
import org.apache.http.io.SessionInputBuffer;
import org.apache.http.io.SessionOutputBuffer;

import org.apache.http.conn.OperatedClientConnection;


/**
 * Default implementation of an operated client connection.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines to avoid svn diff problems -->
 * @version   $Revision: 653041 $ $Date: 2008-05-03 12:39:28 +0200 (Sat, 03 May 2008) $
 *
 * @since 4.0
 */
public class DefaultClientConnection extends SocketHttpClientConnection
    implements OperatedClientConnection {

    private static final Log HEADERS_LOG = LogFactory.getLog("org.apache.http.headers");
    private static final Log WIRE_LOG = LogFactory.getLog("org.apache.http.wire");
    
    private static final Log LOG = LogFactory.getLog(DefaultClientConnection.class);
    
    /** The unconnected socket */
    private volatile Socket socket;

    /** The target host of this connection. */
    private HttpHost targetHost;

    /** Whether this connection is secure. */
    private boolean connSecure;


    // public default constructor


    // non-javadoc, see interface OperatedClientConnection
    public final HttpHost getTargetHost() {
        return this.targetHost;
    }


    // non-javadoc, see interface OperatedClientConnection
    public final boolean isSecure() {
        return this.connSecure;
    }


    @Override
    public final Socket getSocket() {
        return this.socket;
    }


    public void opening(Socket sock, HttpHost target) {
        assertNotOpen();
        this.socket = sock;
        this.targetHost = target;
    }

    
    public void openCompleted(boolean secure, HttpParams params) throws IOException {
        assertNotOpen();
        if (params == null) {
            throw new IllegalArgumentException
                ("Parameters must not be null.");
        }
        this.connSecure = secure;
        bind(this.socket, params);
    }

    /**
     * Force-closes this connection.
     * If the connection is still in the process of being open (the method 
     * {@link #opening opening} was already called but 
     * {@link #openCompleted openCompleted} was not), the associated 
     * socket that is being connected to a remote address will be closed. 
     * That will interrupt a thread that is blocked on connecting 
     * the socket.
     *
     * @throws IOException      in case of a problem
     */
    @Override
    public void shutdown() throws IOException {
        LOG.debug("Connection shut down");
        
        super.shutdown();
        Socket sock = this.socket; // copy volatile attribute
        if (sock != null)
            sock.close();

    } // shutdown

    
    @Override
    public void close() throws IOException {
        LOG.debug("Connection closed");
        super.close();
    }


    @Override
    protected SessionInputBuffer createSessionInputBuffer(
            final Socket socket,
            int buffersize,
            final HttpParams params) throws IOException {
        SessionInputBuffer inbuffer = super.createSessionInputBuffer(
                socket, 
                buffersize,
                params);
        if (WIRE_LOG.isDebugEnabled()) {
            inbuffer = new LoggingSessionInputBuffer(inbuffer, new Wire(WIRE_LOG));
        }
        return inbuffer;
    }

    
    @Override
    protected SessionOutputBuffer createSessionOutputBuffer(
            final Socket socket,
            int buffersize,
            final HttpParams params) throws IOException {
        SessionOutputBuffer outbuffer = super.createSessionOutputBuffer(
                socket,
                buffersize,
                params);
        if (WIRE_LOG.isDebugEnabled()) {
            outbuffer = new LoggingSessionOutputBuffer(outbuffer, new Wire(WIRE_LOG));
        }
        return outbuffer;
    }

    
    @Override
    protected HttpMessageParser createResponseParser(
            final SessionInputBuffer buffer,
            final HttpResponseFactory responseFactory, 
            final HttpParams params) {
        // override in derived class to specify a line parser
        return new DefaultResponseParser
            (buffer, null, responseFactory, params);
    }


    // non-javadoc, see interface OperatedClientConnection
    public void update(Socket sock, HttpHost target,
                       boolean secure, HttpParams params)
        throws IOException {

        assertOpen();
        if (target == null) {
            throw new IllegalArgumentException
                ("Target host must not be null.");
        }
        if (params == null) {
            throw new IllegalArgumentException
                ("Parameters must not be null.");
        }

        if (sock != null) {
            this.socket = sock;
            bind(sock, params);
        }
        targetHost = target;
        connSecure = secure;

    } // update


    @Override
    public HttpResponse receiveResponseHeader() throws HttpException, IOException {
        HttpResponse response = super.receiveResponseHeader();
        if (HEADERS_LOG.isDebugEnabled()) {
            HEADERS_LOG.debug("<< " + response.getStatusLine().toString());
            Header[] headers = response.getAllHeaders();
            for (Header header : headers) {
                HEADERS_LOG.debug("<< " + header.toString());
            }
        }
        return response;
    }


    @Override
    public void sendRequestHeader(HttpRequest request) throws HttpException, IOException {
        super.sendRequestHeader(request);
        if (HEADERS_LOG.isDebugEnabled()) {
            HEADERS_LOG.debug(">> " + request.getRequestLine().toString());
            Header[] headers = request.getAllHeaders();
            for (Header header : headers) {
                HEADERS_LOG.debug(">> " + header.toString());
            }
        }
    }

} // class DefaultClientConnection
