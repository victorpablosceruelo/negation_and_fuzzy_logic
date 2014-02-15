/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha6/module-nio/src/main/java/org/apache/http/impl/nio/reactor/BaseIOReactor.java $
 * $Revision: 581187 $
 * $Date: 2007-10-02 12:54:36 +0200 (Tue, 02 Oct 2007) $
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

import java.io.InterruptedIOException;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.SelectionKey;
import java.util.Iterator;
import java.util.Set;

import org.apache.http.nio.reactor.EventMask;
import org.apache.http.nio.reactor.IOEventDispatch;
import org.apache.http.nio.reactor.IOReactorException;
import org.apache.http.nio.reactor.IOReactorExceptionHandler;
import org.apache.http.nio.reactor.IOSession;

public class BaseIOReactor extends AbstractIOReactor {

    private final long timeoutCheckInterval;
    private SessionSet bufferingSessions;
    
    private long lastTimeoutCheck;
    
    private IOReactorExceptionHandler exceptionHandler = null;
    private IOEventDispatch eventDispatch = null;
    
    public BaseIOReactor(long selectTimeout) throws IOReactorException {
        super(selectTimeout);
        this.bufferingSessions = new SessionSet();
        this.timeoutCheckInterval = selectTimeout;
        this.lastTimeoutCheck = System.currentTimeMillis();
    }

    public void execute(
            final IOEventDispatch eventDispatch) throws InterruptedIOException, IOReactorException {
        if (eventDispatch == null) {
            throw new IllegalArgumentException("Event dispatcher may not be null");
        }
        this.eventDispatch = eventDispatch;
        execute();
    }

    public void setExceptionHandler(IOReactorExceptionHandler exceptionHandler) {
        this.exceptionHandler = exceptionHandler;
    }

    protected void handleRuntimeException(final RuntimeException ex) {
        if (this.exceptionHandler == null || !this.exceptionHandler.handle(ex)) {
            throw ex;
        }
    }

    protected void acceptable(final SelectionKey key) {
    }

    protected void connectable(final SelectionKey key) {
    }

    protected void readable(final SelectionKey key) {
        SessionHandle handle = (SessionHandle) key.attachment();
        IOSession session = handle.getSession();
        handle.resetLastRead();

        try {
            this.eventDispatch.inputReady(session);
        } catch (RuntimeException ex) {
            handleRuntimeException(ex);
        }
        if (session.hasBufferedInput()) {
            this.bufferingSessions.add(session);
        }
    }

    protected void writable(final SelectionKey key) {
        SessionHandle handle = (SessionHandle) key.attachment();
        IOSession session = handle.getSession();
        handle.resetLastWrite();
        
        try {
            this.eventDispatch.outputReady(session);
        } catch (RuntimeException ex) {
            handleRuntimeException(ex);
        }
    }
    
    protected void validate(final Set keys) {
        long currentTime = System.currentTimeMillis();
        if( (currentTime - this.lastTimeoutCheck) >= this.timeoutCheckInterval) {
            this.lastTimeoutCheck = currentTime;
            if (keys != null) {
                for (Iterator it = keys.iterator(); it.hasNext();) {
                    SelectionKey key = (SelectionKey) it.next();
                    timeoutCheck(key, currentTime);
                }
            }
        }
        if (!this.bufferingSessions.isEmpty()) {
            for (Iterator it = this.bufferingSessions.iterator(); it.hasNext(); ) {
                IOSession session = (IOSession) it.next();
                if (!session.hasBufferedInput()) {
                    it.remove();
                    continue;
                }
                try {
                    int ops = session.getEventMask();
                    if ((ops & EventMask.READ) > 0) {
                        try {
                            this.eventDispatch.inputReady(session);
                        } catch (RuntimeException ex) {
                            handleRuntimeException(ex);
                        }
                        if (!session.hasBufferedInput()) {
                            it.remove();
                        }
                    }
                } catch (CancelledKeyException ex) {
                    it.remove();
                }
            }
        }
    }

    protected void timeoutCheck(final SelectionKey key, long now) {
        Object attachment = key.attachment();
        if (attachment instanceof SessionHandle) {
            SessionHandle handle = (SessionHandle) key.attachment();
            IOSession session = handle.getSession();
            int timeout = session.getSocketTimeout();
            if (timeout > 0) {
                if (handle.getLastReadTime() + timeout < now) {
                    try {
                        this.eventDispatch.timeout(session);
                    } catch (RuntimeException ex) {
                        handleRuntimeException(ex);
                    }
                }
            }
        }
    }

    protected void keyCreated(final SelectionKey key, final IOSession session) {
        SessionHandle handle = new SessionHandle(session); 
        key.attach(handle);
        try {
            this.eventDispatch.connected(session);
        } catch (RuntimeException ex) {
            handleRuntimeException(ex);
        }
    }
    
    protected IOSession keyCancelled(final SelectionKey key) {
        Object attachment = key.attachment();
        if (attachment instanceof SessionHandle) {
            SessionHandle handle = (SessionHandle) attachment;
            return handle.getSession();
        } else {
            return null;
        }
    }

    protected void sessionClosed(final IOSession session) {
        try {
            this.eventDispatch.disconnected(session);
        } catch (RuntimeException ex) {
            handleRuntimeException(ex);
        }
    }
    
}
