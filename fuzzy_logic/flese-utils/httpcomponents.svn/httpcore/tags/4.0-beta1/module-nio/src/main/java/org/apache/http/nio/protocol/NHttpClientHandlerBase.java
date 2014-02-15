/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta1/module-nio/src/main/java/org/apache/http/nio/protocol/NHttpClientHandlerBase.java $
 * $Revision: 547966 $
 * $Date: 2007-06-16 21:26:21 +0200 (Sat, 16 Jun 2007) $
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

package org.apache.http.nio.protocol;

import java.io.IOException;

import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpException;
import org.apache.http.nio.NHttpClientConnection;
import org.apache.http.nio.NHttpClientHandler;
import org.apache.http.nio.util.ByteBufferAllocator;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HttpProcessor;

public abstract class NHttpClientHandlerBase extends NHttpHandlerBase 
                                             implements NHttpClientHandler {

    protected HttpRequestExecutionHandler execHandler;
    
    public NHttpClientHandlerBase(
            final HttpProcessor httpProcessor, 
            final HttpRequestExecutionHandler execHandler,
            final ConnectionReuseStrategy connStrategy,
            final ByteBufferAllocator allocator,
            final HttpParams params) {
        super(httpProcessor, connStrategy, allocator, params);
        if (execHandler == null) {
            throw new IllegalArgumentException("HTTP request execution handler may not be null.");
        }
        this.execHandler = execHandler;
    }
    
    public void closed(final NHttpClientConnection conn) {
        if (this.eventListener != null) {
            this.eventListener.connectionClosed(conn);
        }
    }

    public void exception(final NHttpClientConnection conn, final HttpException ex) {
        closeConnection(conn, ex);
        if (this.eventListener != null) {
            this.eventListener.fatalProtocolException(ex, conn);
        }
    }

    public void exception(final NHttpClientConnection conn, final IOException ex) {
        shutdownConnection(conn, ex);
        if (this.eventListener != null) {
            this.eventListener.fatalIOException(ex, conn);
        }
    }
    
}
