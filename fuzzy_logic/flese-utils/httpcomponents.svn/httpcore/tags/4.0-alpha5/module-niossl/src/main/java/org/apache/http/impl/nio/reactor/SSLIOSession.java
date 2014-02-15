/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-niossl/src/main/java/org/apache/http/impl/nio/reactor/SSLIOSession.java $
 * $Revision: 547802 $
 * $Date: 2007-06-15 23:45:59 +0200 (Fri, 15 Jun 2007) $
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
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ByteChannel;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLEngineResult.Status;

import org.apache.http.nio.reactor.EventMask;
import org.apache.http.nio.reactor.IOSession;
import org.apache.http.nio.reactor.SessionBufferStatus;
import org.apache.http.params.HttpParams;

/**
 * A decorator class intended to transparently extend an {@link IOSession} 
 * with transport layer security capabilities based on the SSL/TLS protocol. 
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public class SSLIOSession implements IOSession, SessionBufferStatus {

    private final IOSession session;
    private final SSLEngine sslEngine;
    private final ByteBuffer inEncrypted;
    private final ByteBuffer outEncrypted;
    private final ByteBuffer inPlain;
    private final ByteBuffer outPlain;
    private final InternalByteChannel channel;
    private final SSLIOSessionHandler handler;
    
    private int appEventMask;
    private SessionBufferStatus appBufferStatus;
    
    private volatile boolean closed;
    
    public SSLIOSession(
            final IOSession session, 
            final SSLContext sslContext, 
            final SSLIOSessionHandler handler) {
        super();
        if (session == null) {
            throw new IllegalArgumentException("IO session may not be null");
        }
        if (sslContext == null) {
            throw new IllegalArgumentException("SSL context may not be null");
        }
        this.session = session;
        this.appEventMask = session.getEventMask();
        this.channel = new InternalByteChannel();
        this.handler = handler;
        
        // Override the status buffer interface
        this.session.setBufferStatus(this);
        
        SocketAddress address = session.getRemoteAddress();
        if (address instanceof InetSocketAddress) {
            String hostname = ((InetSocketAddress) address).getHostName();
            int port = ((InetSocketAddress) address).getPort();
            this.sslEngine = sslContext.createSSLEngine(hostname, port);
        } else {
            this.sslEngine = sslContext.createSSLEngine();
        }
        
        // Allocate buffers for network (encrypted) data
        int netBuffersize = this.sslEngine.getSession().getPacketBufferSize();
        this.inEncrypted = ByteBuffer.allocateDirect(netBuffersize);
        this.outEncrypted = ByteBuffer.allocateDirect(netBuffersize);

        // Allocate buffers for application (unencrypted) data
        int appBuffersize = this.sslEngine.getSession().getApplicationBufferSize();
        this.inPlain = ByteBuffer.allocateDirect(appBuffersize);
        this.outPlain = ByteBuffer.allocateDirect(appBuffersize);
    }
    
    public synchronized void bind(
            final SSLMode mode, 
            final HttpParams params) throws SSLException {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        switch (mode) {
        case CLIENT:
            this.sslEngine.setUseClientMode(true);
            break;
        case SERVER:
            this.sslEngine.setUseClientMode(false);
            break;
        }
        if (this.handler != null) {
            this.handler.initalize(this.sslEngine, params);
        }
        this.sslEngine.beginHandshake();
        doHandshake();
    }
    
    private void doHandshake() throws SSLException {
        boolean handshaking = true;
        
        SSLEngineResult result = null;
        while (handshaking) {
            switch (this.sslEngine.getHandshakeStatus()) {
            case NEED_WRAP:
                // Generate outgoing handshake data
                this.outPlain.flip();
                result = this.sslEngine.wrap(this.outPlain, this.outEncrypted);
                this.outPlain.compact();
                if (result.getStatus() != Status.OK) {
                    handshaking = false;
                }
                if (result.getStatus() == Status.CLOSED) {
                    this.closed = true;
                }
                break;
            case NEED_UNWRAP:
                // Process incoming handshake data
                this.inEncrypted.flip();
                result = this.sslEngine.unwrap(this.inEncrypted, this.inPlain);
                this.inEncrypted.compact();
                if (result.getStatus() != Status.OK) {
                    handshaking = false;
                }
                if (result.getStatus() == Status.CLOSED) {
                    this.closed = true;
                }
                break;
            case NEED_TASK:
                Runnable r = this.sslEngine.getDelegatedTask();
                r.run();
                break;
            case NOT_HANDSHAKING:
                handshaking = false;
                break;
            }
        }

        // The SSLEngine has just finished handshaking. This value is only generated by a call
        // to SSLEngine.wrap()/unwrap() when that call finishes a handshake.
        // It is never generated by SSLEngine.getHandshakeStatus(). 
        if (result != null && result.getHandshakeStatus() == HandshakeStatus.FINISHED) {
            if (this.handler != null) {
                this.handler.verify(
                        this.session.getRemoteAddress(),
                        this.sslEngine.getSession());
            }
        }
    }

    private void updateEventMask() {
        // Need to toggle the event mask for this channel?
        int oldMask = this.session.getEventMask();
        int newMask = oldMask;
        switch (this.sslEngine.getHandshakeStatus()) {
        case NEED_WRAP:
            newMask = EventMask.READ_WRITE;
            break;
        case NEED_UNWRAP:
            newMask = EventMask.READ;
            break;
        case NOT_HANDSHAKING:
            newMask = this.appEventMask;
            break;
        }
        
        // Do we have encrypted data ready to be sent?
        if (this.outEncrypted.position() > 0) {
            newMask = newMask | EventMask.WRITE;
        }

        // Update the mask if necessary
        if (oldMask != newMask) {
            this.session.setEventMask(newMask);
        }
    }
    
    private int sendEncryptedData() throws IOException {
        this.outEncrypted.flip();
        int bytesWritten = this.session.channel().write(this.outEncrypted);
        this.outEncrypted.compact();
        
        if (this.sslEngine.isInboundDone() && this.sslEngine.isOutboundDone()) {
            this.session.close();
        }
        return bytesWritten;
    }

    private int receiveEncryptedData() throws IOException {
        int bytesRead = this.session.channel().read(this.inEncrypted);
        if (bytesRead == -1) {
            this.session.close();
        }
        if (this.sslEngine.isInboundDone() && this.sslEngine.isOutboundDone()) {
            this.session.close();
        }
        return bytesRead;
    }
    
    private boolean decryptData() throws SSLException {
        boolean decrypted = false;
        if (this.inEncrypted.position() > 0) {
            this.inEncrypted.flip();
            SSLEngineResult result = this.sslEngine.unwrap(this.inEncrypted, this.inPlain);
            this.inEncrypted.compact();
            if (result.getStatus() == Status.CLOSED) {
                this.closed = true;
            }
            if (result.getStatus() == Status.OK) {
                decrypted = true;
            }
        }
        return decrypted;
    }

    public synchronized boolean isAppInputReady() throws IOException {
        int bytesRead = receiveEncryptedData();
        if (bytesRead == -1) {
            this.closed = true;
            return false;
        }
        doHandshake();
        decryptData();
        return this.session.hasBufferedInput() || this.inPlain.position() > 0;
    }
    
    public synchronized boolean isAppOutputReady() throws IOException {
        return !this.closed
            && this.sslEngine.getHandshakeStatus() == HandshakeStatus.NOT_HANDSHAKING;
    }
    
    public synchronized void inboundTransport() throws IOException {
        updateEventMask();
    }
    
    public synchronized void outboundTransport() throws IOException {
        sendEncryptedData();
        doHandshake();
        updateEventMask();
    }
    
    private synchronized int wrap(final ByteBuffer src) throws SSLException {
        if (src == null) {
            throw new IllegalArgumentException("Byte buffer may not be null");
        }
        if (this.closed) {
            return -1;
        }
        if (this.outPlain.position() > 0) {
            this.outPlain.flip();
            this.sslEngine.wrap(this.outPlain, this.outEncrypted);
            this.outPlain.compact();
        }
        if (this.outPlain.position() == 0) {
            SSLEngineResult result = this.sslEngine.wrap(src, this.outEncrypted);
            if (result.getStatus() == Status.CLOSED) {
                this.closed = true;
            }
            return result.bytesConsumed();
        } else {
            return 0;
        }
    }
    
    private synchronized int unwrap(final ByteBuffer dst) throws SSLException {
        if (dst == null) {
            throw new IllegalArgumentException("Byte buffer may not be null");
        }
        if (this.closed) {
            return -1;
        }
        if (this.inPlain.position() > 0) {
            this.inPlain.flip();
            int n = Math.min(this.inPlain.remaining(), dst.remaining());
            for (int i = 0; i < n; i++) {
                dst.put(this.inPlain.get());
            }
            this.inPlain.compact();
            return n; 
        } else {
            return 0;
        }
    }
    
    public void close() {
        if (this.closed) {
            return;
        }
        this.closed = true;
        synchronized(this) {
            this.sslEngine.closeOutbound();
            updateEventMask();
        }
    }

    public boolean isClosed() {
        return this.closed;
    }

    public synchronized boolean isInboundDone() {
        return this.sslEngine.isInboundDone();
    }
    
    public synchronized boolean isOutboundDone() {
        return this.sslEngine.isOutboundDone();
    }
    
    public void shutdown() {
        this.closed = true;
        this.session.shutdown();
    }

    public ByteChannel channel() {
        return this.channel;
    }

    public SocketAddress getLocalAddress() {
        return this.session.getLocalAddress();
    }

    public SocketAddress getRemoteAddress() {
        return this.session.getRemoteAddress();
    }

    public synchronized int getEventMask() {
        return this.appEventMask;
    }

    public synchronized void setEventMask(int ops) {
        this.appEventMask = ops;
        updateEventMask();
    }

    public synchronized void setEvent(int op) {
        this.appEventMask = this.appEventMask | op;
        updateEventMask();
    }

    public synchronized void clearEvent(int op) {
        this.appEventMask = this.appEventMask & ~op;
        updateEventMask();
    }

    public int getSocketTimeout() {
        return this.session.getSocketTimeout();
    }

    public void setSocketTimeout(int timeout) {
        this.session.setSocketTimeout(timeout);
    }

    public synchronized boolean hasBufferedInput() {
        return this.appBufferStatus.hasBufferedInput()
            || this.inEncrypted.position() > 0
            || this.inPlain.position() > 0;
    }

    public synchronized boolean hasBufferedOutput() {
        return this.appBufferStatus.hasBufferedOutput()
            || this.outEncrypted.position() > 0
            || this.outPlain.position() > 0;
    }

    public synchronized void setBufferStatus(final SessionBufferStatus status) {
        this.appBufferStatus = status;
    }

    public Object getAttribute(final String name) {
        return this.session.getAttribute(name);
    }

    public Object removeAttribute(final String name) {
        return this.session.removeAttribute(name);
    }

    public void setAttribute(final String name, final Object obj) {
        this.session.setAttribute(name, obj);
    }
    
    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append(this.session);
        buffer.append("[SSL handshake status: ");
        buffer.append(this.sslEngine.getHandshakeStatus());
        buffer.append("][");
        buffer.append(this.inEncrypted.position());
        buffer.append("][");
        buffer.append(this.inPlain.position());
        buffer.append("][");
        buffer.append(this.outEncrypted.position());
        buffer.append("][");
        buffer.append(this.outPlain.position());
        buffer.append("]");
        return buffer.toString();
    }

    private class InternalByteChannel implements ByteChannel {

        public int write(final ByteBuffer src) throws IOException {
            return wrap(src);
        }

        public int read(final ByteBuffer dst) throws IOException {
            return unwrap(dst);
        }

        public void close() throws IOException {
            close();
        }

        public boolean isOpen() {
            return !isClosed();
        }
        
    }

}
