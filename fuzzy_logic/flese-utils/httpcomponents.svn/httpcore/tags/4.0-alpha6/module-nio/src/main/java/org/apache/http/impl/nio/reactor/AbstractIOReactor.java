/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha6/module-nio/src/main/java/org/apache/http/impl/nio/reactor/AbstractIOReactor.java $
 * $Revision: 581638 $
 * $Date: 2007-10-03 17:50:37 +0200 (Wed, 03 Oct 2007) $
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
import java.io.InterruptedIOException;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.Channel;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.util.Set;

import org.apache.http.nio.reactor.IOReactor;
import org.apache.http.nio.reactor.IOReactorException;
import org.apache.http.nio.reactor.IOSession;

public abstract class AbstractIOReactor implements IOReactor {

    private volatile int status;
    
    private final Object shutdownMutex;
    private final long selectTimeout;
    private final Selector selector;
    private final SessionSet sessions;
    private final SessionQueue closedSessions;
    private final ChannelQueue newChannels;
    
    public AbstractIOReactor(long selectTimeout) throws IOReactorException {
        super();
        if (selectTimeout <= 0) {
            throw new IllegalArgumentException("Select timeout may not be negative or zero");
        }
        this.selectTimeout = selectTimeout;
        this.sessions = new SessionSet();
        this.closedSessions = new SessionQueue();
        this.newChannels = new ChannelQueue();
        try {
            this.selector = Selector.open();
        } catch (IOException ex) {
            throw new IOReactorException("Failure opening selector", ex);
        }
        this.shutdownMutex = new Object();
        this.status = INACTIVE;
    }

    protected abstract void acceptable(SelectionKey key);
    
    protected abstract void connectable(SelectionKey key);

    protected abstract void readable(SelectionKey key);

    protected abstract void writable(SelectionKey key);
    
    protected abstract void timeoutCheck(SelectionKey key, long now);

    protected abstract void validate(Set keys);
    
    protected abstract void keyCreated(SelectionKey key, IOSession session);
    
    protected abstract IOSession keyCancelled(SelectionKey key);
    
    protected abstract void sessionClosed(IOSession session);
    
    public int getStatus() {
        return this.status;
    }

    public void addChannel(final ChannelEntry channelEntry) {
        if (channelEntry == null) {
            throw new IllegalArgumentException("Channel entry may not be null");
        }
        this.newChannels.push(channelEntry);
        this.selector.wakeup();
    }
    
    protected void execute() throws InterruptedIOException, IOReactorException {
        this.status = ACTIVE;

        try {
            for (;;) {
                
                int readyCount;
                try {
                    readyCount = this.selector.select(this.selectTimeout);
                } catch (InterruptedIOException ex) {
                    throw ex;
                } catch (IOException ex) {
                    throw new IOReactorException("Unexpected selector failure", ex);
                }
                
                if (this.status == SHUT_DOWN) {
                    // Hard shut down. Exit select loop immediately
                    break;
                }

                if (this.status == SHUTTING_DOWN) {
                    // Graceful shutdown in process
                    // Try to close things out nicely
                    closeSessions();
                    closeNewChannels();
                }
                
                // Process selected I/O events 
                if (readyCount > 0) {
                    processEvents(this.selector.selectedKeys());
                }
                
                // Validate active channels
                validate(this.selector.keys());
                
                // Process closed sessions
                processClosedSessions();

                // If active process new channels
                if (this.status == ACTIVE) {
                    processNewChannels();
                }
                
                // Exit select loop if graceful shutdown has been completed
                if (this.status > ACTIVE && this.sessions.isEmpty()) {
                    break;
                }
                
            }
            
            // Close remaining active channels and the selector itself
            closeActiveChannels();
            
        } catch (ClosedSelectorException ex) {
        } finally {
            synchronized (this.shutdownMutex) {
                this.status = SHUT_DOWN;
                this.shutdownMutex.notifyAll();
            }
        }
    }
    
    private void processEvents(final Set selectedKeys) {
        for (Iterator it = selectedKeys.iterator(); it.hasNext(); ) {
            
            SelectionKey key = (SelectionKey) it.next();
            processEvent(key);
            
        }
        selectedKeys.clear();
    }

    protected void processEvent(final SelectionKey key) {
        try {
            if (key.isAcceptable()) {
                acceptable(key);
            }
            if (key.isConnectable()) {
                connectable(key);
            }
            if (key.isReadable()) {
                readable(key);
            }
            if (key.isWritable()) {
                writable(key);
            }
        } catch (CancelledKeyException ex) {
            IOSession session = keyCancelled(key);
            if (session != null) {
                this.closedSessions.push(session);
            }
            key.attach(null);
        }
    }

    private void processNewChannels() throws IOReactorException {
        ChannelEntry entry;
        while ((entry = this.newChannels.pop()) != null) {
            
            SocketChannel channel;
            SelectionKey key;
            try {
                channel = entry.getChannel();
                channel.configureBlocking(false);
                key = channel.register(this.selector, 0);
            } catch (IOException ex) {
                throw new IOReactorException("Failure registering channel " +
                        "with the selector", ex);
            }

            IOSession session = new IOSessionImpl(key, new SessionClosedCallback() {

                public void sessionClosed(IOSession session) {
                    closedSessions.push(session);
                }
                
            });
            
            int timeout = 0;
            try {
                timeout = channel.socket().getSoTimeout();
            } catch (IOException ex) {
                // Very unlikely to happen and is not fatal
                // as the protocol layer is expected to overwrite
                // this value anyways
            }
            
            session.setAttribute(IOSession.ATTACHMENT_KEY, entry.getAttachment());
            session.setSocketTimeout(timeout);
            this.sessions.add(session);

            try {
                keyCreated(key, session);
                
                SessionRequestImpl sessionRequest = entry.getSessionRequest();
                if (sessionRequest != null) {
                    sessionRequest.completed(session);
                }
            } catch (CancelledKeyException ex) {
                this.closedSessions.push(session);
                key.attach(null);
            }
        }
    }

    private void processClosedSessions() {
        IOSession session;
        while ((session = this.closedSessions.pop()) != null) {
            if (this.sessions.remove(session)) {
                sessionClosed(session);
            }
        }
    }

    protected void closeSessions() {
        synchronized (this.sessions) {
            for (Iterator it = this.sessions.iterator(); it.hasNext(); ) {
                IOSession session = (IOSession) it.next();
                session.close();
            }
        }
    }
    
    protected void closeNewChannels() throws IOReactorException {
        ChannelEntry entry;
        while ((entry = this.newChannels.pop()) != null) {
            SessionRequestImpl sessionRequest = entry.getSessionRequest();
            if (sessionRequest != null) {
                sessionRequest.cancel();
            }
            SocketChannel channel = entry.getChannel();
            try {
                channel.close();
            } catch (IOException ignore) {
            }
        }
    }
    
    protected void closeActiveChannels() throws IOReactorException {
        Set keys = this.selector.keys();
        for (Iterator it = keys.iterator(); it.hasNext(); ) {
            try {
                SelectionKey key = (SelectionKey) it.next();
                Channel channel = key.channel();
                if (channel != null) {
                    channel.close();
                }
            } catch (IOException ignore) {
            }
        }
        try {
            this.selector.close();
        } catch (IOException ignore) {
        }
    }
    
    public void gracefulShutdown() {
        if (this.status != ACTIVE) {
            // Already shutting down
            return;
        }
        this.status = SHUTTING_DOWN;
        this.selector.wakeup();
    }
        
    public void hardShutdown() throws IOReactorException {
        if (this.status == SHUT_DOWN) {
            // Already shut down
            return;
        }
        this.status = SHUT_DOWN;
        closeNewChannels();
        closeActiveChannels();
    }
    
    public void awaitShutdown(long timeout) throws InterruptedException {
        synchronized (this.shutdownMutex) {
            long deadline = System.currentTimeMillis() + timeout;
            long remaining = timeout;
            while (this.status != SHUT_DOWN) {
                this.shutdownMutex.wait(remaining);
                if (timeout > 0) {
                    remaining = deadline - System.currentTimeMillis();
                    if (remaining <= 0) {
                        break;
                    }
                }
            }
        }
    }
        
    public void shutdown(long gracePeriod) throws IOReactorException {
        if (this.status != INACTIVE) {
            gracefulShutdown();
            try {
                awaitShutdown(gracePeriod);
            } catch (InterruptedException ignore) {
            }
        }
        if (this.status != SHUT_DOWN) {
            hardShutdown();
        }
    }
    
    public void shutdown() throws IOReactorException {
        shutdown(1000);
    }
    
}
