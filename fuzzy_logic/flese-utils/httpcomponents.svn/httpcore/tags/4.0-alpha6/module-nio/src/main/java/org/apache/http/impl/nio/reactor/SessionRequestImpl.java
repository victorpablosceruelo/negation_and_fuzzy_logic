/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha6/module-nio/src/main/java/org/apache/http/impl/nio/reactor/SessionRequestImpl.java $
 * $Revision: 534612 $
 * $Date: 2007-05-02 22:49:54 +0200 (Wed, 02 May 2007) $
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

package org.apache.http.impl.nio.reactor;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.channels.SelectionKey;

import org.apache.http.nio.reactor.IOSession;
import org.apache.http.nio.reactor.SessionRequest;
import org.apache.http.nio.reactor.SessionRequestCallback;

public class SessionRequestImpl implements SessionRequest {

    private volatile boolean completed;
    private volatile SelectionKey key;

    private final SocketAddress remoteAddress;
    private final SocketAddress localAddress;
    private final Object attachment;
    private final SessionRequestCallback callback;
    
    private int connectTimeout;
    private IOSession session = null;
    private IOException exception = null;
    
    public SessionRequestImpl(
            final SocketAddress remoteAddress,
            final SocketAddress localAddress,
            final Object attachment,
            final SessionRequestCallback callback) {
        super();
        if (remoteAddress == null) {
            throw new IllegalArgumentException("Remote address may not be null");
        }
        this.remoteAddress = remoteAddress;
        this.localAddress = localAddress;
        this.attachment = attachment;
        this.callback = callback;
        this.connectTimeout = 0;
    }
    
    public SocketAddress getRemoteAddress() {
        return this.remoteAddress;
    }
    
    public SocketAddress getLocalAddress() {
        return this.localAddress;
    }
    
    public Object getAttachment() {
        return this.attachment;
    }
    
    public boolean isCompleted() {
        return this.completed;
    }
    
    public void waitFor() throws InterruptedException {
        if (this.completed) {
            return;
        }
        synchronized (this) {
            while (!this.completed) {
                wait();
            }
        }
    }
    
    public IOSession getSession() {
        synchronized (this) {
            return this.session;
        }
    }
    
    public IOException getException() {
        synchronized (this) {
            return this.exception;
        }
    }
    
    public void completed(final IOSession session) {
        if (session == null) {
            throw new IllegalArgumentException("Session may not be null");
        }
        if (this.completed) {
            return;
        }
        this.completed = true;
        synchronized (this) {
            this.session = session;
            if (this.callback != null) {
                this.callback.completed(this);
            }
            notifyAll();
        }
    }
 
    public void failed(final IOException exception) {
        if (exception == null) {
            return;
        }
        if (this.completed) {
            return;
        }
        this.completed = true;
        synchronized (this) {
            this.exception = exception;
            if (this.callback != null) {
                this.callback.failed(this);
            }
            notifyAll();
        }
    }
 
    public void timeout() {
        if (this.completed) {
            return;
        }
        synchronized (this) {
            if (this.callback != null) {
                this.callback.timeout(this);
            }
        }
    }
 
    public int getConnectTimeout() {
        return this.connectTimeout;
    }

    public void setConnectTimeout(int timeout) {
        if (this.connectTimeout != timeout) {
            this.connectTimeout = timeout;
            if (this.key != null) {
                this.key.selector().wakeup();
            }
        }
    }

    protected void setKey(final SelectionKey key) {
        this.key = key;
    }

    public void cancel() {
        if (this.completed) {
            return;
        }
        this.completed = true;
        if (this.key != null) {
            this.key.cancel();
        }
        synchronized (this) {
            if (this.callback != null) {
                this.callback.cancelled(this);
            }
            notifyAll();
        }
    }
    
}
