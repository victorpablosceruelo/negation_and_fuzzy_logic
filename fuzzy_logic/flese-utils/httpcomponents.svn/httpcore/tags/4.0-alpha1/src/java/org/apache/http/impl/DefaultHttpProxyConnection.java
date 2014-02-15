/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha1/src/java/org/apache/http/impl/DefaultHttpProxyConnection.java $
 * $Revision: 376961 $
 * $Date: 2006-02-11 11:32:50 +0100 (Sat, 11 Feb 2006) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2006 The Apache Software Foundation
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
 */

package org.apache.http.impl;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;

import org.apache.http.HttpHost;
import org.apache.http.HttpProxyConnection;
import org.apache.http.Scheme;
import org.apache.http.ProxyHost;
import org.apache.http.io.SecureSocketFactory;
import org.apache.http.io.SocketFactory;
import org.apache.http.params.HttpParams;

/**
 * Default implementation of a client-side connection through a proxy.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 376961 $
 * 
 * @since 4.0
 */
public class DefaultHttpProxyConnection 
        extends DefaultHttpClientConnection implements HttpProxyConnection {

    private volatile HttpHost tunneltarget = null;
    private volatile boolean secure = false;
    
    public DefaultHttpProxyConnection(final ProxyHost proxyhost, final InetAddress localAddress) {
        super(proxyhost, localAddress);
    }
    
    public DefaultHttpProxyConnection(final ProxyHost proxyhost) {
        this(proxyhost, null);
    }

    public void close() throws IOException {
        this.tunneltarget = null;
        this.secure = false;
        super.close();
    }
    
    public void tunnelTo(final HttpHost targetHost, final HttpParams params) 
            throws IOException {
        if (targetHost == null) {
            throw new IllegalArgumentException("Target host may not be null");
        }
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        if (this.tunneltarget != null) {
            throw new IllegalStateException("Secure tunnel to " + 
                    this.tunneltarget + " is already active");
        }
        assertOpen();
        Scheme protocol = targetHost.getScheme();
        SocketFactory socketfactory = protocol.getSocketFactory();
        if (socketfactory instanceof SecureSocketFactory) {
            Socket socket = ((SecureSocketFactory)socketfactory)
                .createSocket(
                    this.socket, 
                    targetHost.getHostName(), 
                    targetHost.getPort(), 
                    true);
            bind(socket, params);
            this.secure = true;
        } else {
            this.secure = false;
        }
        this.tunneltarget = targetHost;
    }
    
    public HttpHost getTunnelTarget() {
        return this.tunneltarget;
    }
    
    public boolean isTunnelActive() {
        return this.tunneltarget != null;
    }
    
    public boolean isSecure() {
        return this.secure;
    }
}
