/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-nio/src/test/java/org/apache/http/nio/mockup/TestHttpServiceBase.java $
 * $Revision: 505893 $
 * $Date: 2007-02-11 12:31:59 +0100 (Sun, 11 Feb 2007) $
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

package org.apache.http.nio.mockup;

import java.io.IOException;
import java.net.InetAddress;

import org.apache.http.HttpException;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.nio.protocol.EventListener;
import org.apache.http.nio.reactor.IOReactor;
import org.apache.http.params.HttpParams;

abstract class TestHttpServiceBase {

    private volatile IOReactorThread thread;
    private volatile int connCount = 0;
    private final Object mutex;
    
    protected final HttpParams params;
    protected IOReactor ioReactor;
    
    public TestHttpServiceBase() {
        super();
        this.mutex = new Object();
        this.params = new BasicHttpParams(null);
    }
    
    protected abstract void execute() throws IOException;
    
    public void start() {
        this.thread = new IOReactorThread();
        this.thread.start();
    }
    
    public void shutdown() throws IOException {
        this.ioReactor.shutdown();
        try {
            this.thread.join(500);
        } catch (InterruptedException ignore) {
        }
    }
    
    public int getConnCount() {
        return this.connCount;
    }
    
    public HttpParams getParams() {
        return this.params;
    }
    
    private void incrementConnCount() {
        synchronized (this.mutex) {
            this.connCount++;
            this.mutex.notifyAll();
        }
    }
    
    public void await(int connCount, long timeout) throws InterruptedException {
        synchronized (this.mutex) {
            while (this.connCount < connCount) {
                this.mutex.wait(timeout);
            }
        }
    }
    
    private class IOReactorThread extends Thread {

        public void run() {
            try {
                execute();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }

    }    

    protected class EventLogger implements EventListener {

        public void connectionOpen(final InetAddress address) {
        }

        public void connectionTimeout(final InetAddress address) {
        }

        public void connectionClosed(InetAddress address) {
            incrementConnCount();
        }

        public void fatalIOException(IOException ex) {
            ex.printStackTrace();
        }

        public void fatalProtocolException(HttpException ex) {
            ex.printStackTrace();
        }
        
    }
        
}
