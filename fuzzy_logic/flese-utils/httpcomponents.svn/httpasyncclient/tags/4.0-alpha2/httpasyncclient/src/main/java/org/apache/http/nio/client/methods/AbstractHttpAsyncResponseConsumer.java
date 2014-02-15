/*
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
package org.apache.http.nio.client.methods;

import java.io.IOException;

import org.apache.http.HttpException;
import org.apache.http.HttpResponse;
import org.apache.http.nio.ContentDecoder;
import org.apache.http.nio.IOControl;
import org.apache.http.nio.client.HttpAsyncResponseConsumer;

public abstract class AbstractHttpAsyncResponseConsumer<T> implements HttpAsyncResponseConsumer<T> {

    private volatile HttpResponse response;
    private volatile Exception ex;
    private volatile boolean completed;
    private volatile T result;

    public AbstractHttpAsyncResponseConsumer() {
        super();
    }

    protected abstract void onResponseReceived(final HttpResponse response);

    protected abstract void onContentReceived(
            final ContentDecoder decoder, final IOControl ioctrl) throws IOException;

    protected abstract void onCleanup();

    protected abstract T buildResult() throws Exception;

    void releaseResources() {
        onCleanup();
    }

    public synchronized void responseReceived(final HttpResponse response) throws IOException, HttpException {
        if (this.response != null) {
            throw new IllegalStateException("HTTP response already set");
        }
        this.response = response;
        onResponseReceived(this.response);
    }

    public synchronized void consumeContent(
            final ContentDecoder decoder, final IOControl ioctrl) throws IOException {
        onContentReceived(decoder, ioctrl);
    }

    public synchronized void responseCompleted() {
        if (this.completed) {
            return;
        }
        this.completed = true;
        try {
            this.result = buildResult();
        } catch (Exception ex) {
            this.ex = ex;
        } finally {
            releaseResources();
        }
    }

    public synchronized void cancel() {
        if (this.completed) {
            return;
        }
        this.completed = true;
        this.response = null;
        releaseResources();
    }

    public synchronized void failed(final Exception ex) {
        if (this.completed) {
            return;
        }
        this.completed = true;
        this.ex = ex;
        this.response = null;
        releaseResources();
    }

    public Exception getException() {
        return this.ex;
    }

    public T getResult() {
        return this.result;
    }

}
