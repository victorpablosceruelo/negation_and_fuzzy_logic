/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/impl/SimpleHttpAsyncClientConnection.java $
 * $Revision: 489367 $
 * $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
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

package org.apache.http.async.impl;

import java.io.IOException;
import java.net.Socket;

import org.apache.http.HttpHost;
import org.apache.http.async.HttpAsyncClientConnection;
import org.apache.http.impl.SocketHttpClientConnection;
import org.apache.http.params.HttpParams;

/**
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public class SimpleHttpAsyncClientConnection 
    extends SocketHttpClientConnection implements HttpAsyncClientConnection {

    private HttpHost targethost = null;
    
    public SimpleHttpAsyncClientConnection(final HttpHost targethost) {
        super();
        this.targethost = targethost;
    }
    
    public SimpleHttpAsyncClientConnection() {
        this(null);
    }
    
    public void open(final HttpParams params) throws IOException {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        if (this.targethost == null) {
            throw new IllegalStateException("Target host not specified");
        }
        assertNotOpen();
        String hostname = this.targethost.getHostName();
        int port = this.targethost.getPort();
        if (port == -1) {
            port = 80;
        }
        Socket socket = new Socket(hostname, port);
        bind(socket, params);
    }    

    public HttpHost getTargetHost() {
        return this.targethost;
    }
    
    public void setTargetHost(final HttpHost targethost) {
        if (targethost == null) {
            throw new IllegalArgumentException("Target host may not be null");
        }
        assertNotOpen();
        this.targethost = targethost;
    }
    
}
