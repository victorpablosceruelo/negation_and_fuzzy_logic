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

package org.apache.http.nio.protocol;

import java.io.IOException;
import java.util.concurrent.Future;

import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.concurrent.BasicFuture;
import org.apache.http.concurrent.FutureCallback;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.nio.ContentDecoder;
import org.apache.http.nio.ContentEncoder;
import org.apache.http.nio.IOControl;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpProcessor;
import org.apache.http.util.Args;

/**
 * Basic implementation of {@link HttpAsyncRequestExecutionHandler} that executes
 * a single HTTP request / response exchange.
 *
 * @param <T> the result type of request execution.
 * @since 4.2
 */
@SuppressWarnings("deprecation")
public class BasicAsyncRequestExecutionHandler<T> implements HttpAsyncRequestExecutionHandler<T> {

    private final HttpAsyncRequestProducer requestProducer;
    private final HttpAsyncResponseConsumer<T> responseConsumer;
    private final BasicFuture<T> future;
    private final HttpContext localContext;
    private final HttpProcessor httppocessor;
    private final ConnectionReuseStrategy reuseStrategy;

    private volatile boolean requestSent;

    /**
     * @deprecated (4.3) use
     *   {@link BasicAsyncRequestExecutionHandler#BasicAsyncRequestExecutionHandler(
     *     HttpAsyncRequestProducer, HttpAsyncResponseConsumer, FutureCallback, HttpContext,
     *     HttpProcessor, ConnectionReuseStrategy)}
     */
    @Deprecated
    public BasicAsyncRequestExecutionHandler(
            final HttpAsyncRequestProducer requestProducer,
            final HttpAsyncResponseConsumer<T> responseConsumer,
            final FutureCallback<T> callback,
            final HttpContext localContext,
            final HttpProcessor httppocessor,
            final ConnectionReuseStrategy reuseStrategy,
            final HttpParams params) {
        super();
        Args.notNull(requestProducer, "Request producer");
        Args.notNull(responseConsumer, "Response consumer");
        Args.notNull(localContext, "HTTP context");
        Args.notNull(httppocessor, "HTTP processor");
        Args.notNull(reuseStrategy, "Connection reuse strategy");
        Args.notNull(params, "HTTP parameters");
        this.requestProducer = requestProducer;
        this.responseConsumer = responseConsumer;
        this.future = new BasicFuture<T>(callback);
        this.localContext = localContext;
        this.httppocessor = httppocessor;
        this.reuseStrategy = reuseStrategy;
    }

    /**
     * @deprecated (4.3) use
     *   {@link BasicAsyncRequestExecutionHandler#BasicAsyncRequestExecutionHandler(
     *     HttpAsyncRequestProducer, HttpAsyncResponseConsumer, FutureCallback, HttpContext,
     *     HttpProcessor, ConnectionReuseStrategy)}
     */
    @Deprecated
    public BasicAsyncRequestExecutionHandler(
            final HttpAsyncRequestProducer requestProducer,
            final HttpAsyncResponseConsumer<T> responseConsumer,
            final HttpContext localContext,
            final HttpProcessor httppocessor,
            final ConnectionReuseStrategy reuseStrategy,
            final HttpParams params) {
        this(requestProducer, responseConsumer, null, localContext, httppocessor, reuseStrategy, params);
    }

    /**
     * Creates new instance of BasicAsyncRequestExecutionHandler.
     *
     * @param requestProducer the request producer.
     * @param responseConsumer the response consumer.
     * @param callback the future callback invoked when the operation is completed.
     * @param localContext the local execution context.
     * @param httppocessor the HTTP protocol processor.
     * @param reuseStrategy the connection re-use strategy. If <code>null</code>
     *   {@link DefaultConnectionReuseStrategy#INSTANCE} will be used.
     *
     * @since 4.3
     */
    public BasicAsyncRequestExecutionHandler(
            final HttpAsyncRequestProducer requestProducer,
            final HttpAsyncResponseConsumer<T> responseConsumer,
            final FutureCallback<T> callback,
            final HttpContext localContext,
            final HttpProcessor httppocessor,
            final ConnectionReuseStrategy reuseStrategy) {
        super();
        this.requestProducer = Args.notNull(requestProducer, "Request producer");
        this.responseConsumer = Args.notNull(responseConsumer, "Response consumer");
        this.future = new BasicFuture<T>(callback);
        this.localContext = Args.notNull(localContext, "HTTP context");
        this.httppocessor = Args.notNull(httppocessor, "HTTP processor");
        this.reuseStrategy = reuseStrategy != null ? reuseStrategy :
            DefaultConnectionReuseStrategy.INSTANCE;
    }

    /**
     * Creates new instance of BasicAsyncRequestExecutionHandler.
     *
     * @param requestProducer the request producer.
     * @param responseConsumer the response consumer.
     * @param localContext the local execution context.
     * @param httppocessor the HTTP protocol processor.
     *
     * @since 4.3
     */
    public BasicAsyncRequestExecutionHandler(
            final HttpAsyncRequestProducer requestProducer,
            final HttpAsyncResponseConsumer<T> responseConsumer,
            final HttpContext localContext,
            final HttpProcessor httppocessor) {
        this(requestProducer, responseConsumer, null, localContext, httppocessor, null);
    }

    public Future<T> getFuture() {
        return this.future;
    }

    private void releaseResources() {
        try {
            this.responseConsumer.close();
        } catch (IOException ex) {
        }
        try {
            this.requestProducer.close();
        } catch (IOException ex) {
        }
    }

    public void close() throws IOException {
        releaseResources();
        if (!this.future.isDone()) {
            this.future.cancel();
        }
    }

    public HttpHost getTarget() {
        return this.requestProducer.getTarget();
    }

    public HttpRequest generateRequest() throws IOException, HttpException {
        return this.requestProducer.generateRequest();
    }

    public void produceContent(
            final ContentEncoder encoder, final IOControl ioctrl) throws IOException {
        this.requestProducer.produceContent(encoder, ioctrl);
    }

    public void requestCompleted(final HttpContext context) {
        this.requestProducer.requestCompleted(context);
        this.requestSent = true;
    }

    public boolean isRepeatable() {
        return false;
    }

    public void resetRequest() {
    }

    public void responseReceived(final HttpResponse response) throws IOException, HttpException {
        this.responseConsumer.responseReceived(response);
    }

    public void consumeContent(
            final ContentDecoder decoder, final IOControl ioctrl) throws IOException {
        this.responseConsumer.consumeContent(decoder, ioctrl);
    }

    public void failed(final Exception ex) {
        try {
            if (!this.requestSent) {
                this.requestProducer.failed(ex);
            }
            this.responseConsumer.failed(ex);
        } finally {
            try {
                this.future.failed(ex);
            } finally {
                releaseResources();
            }
        }
    }

    public boolean cancel() {
        try {
            boolean cancelled = this.responseConsumer.cancel();
            this.future.cancel();
            releaseResources();
            return cancelled;
        } catch (RuntimeException ex) {
            failed(ex);
            throw ex;
        }
    }

    public void responseCompleted(final HttpContext context) {
        try {
            this.responseConsumer.responseCompleted(context);
            T result = this.responseConsumer.getResult();
            Exception ex = this.responseConsumer.getException();
            if (ex == null) {
                this.future.completed(result);
            } else {
                this.future.failed(ex);
            }
            releaseResources();
        } catch (RuntimeException ex) {
            failed(ex);
            throw ex;
        }
    }

    public T getResult() {
        return this.responseConsumer.getResult();
    }

    public Exception getException() {
        return this.responseConsumer.getException();
    }

    public HttpContext getContext() {
        return this.localContext;
    }

    public HttpProcessor getHttpProcessor() {
        return this.httppocessor;
    }

    public ConnectionReuseStrategy getConnectionReuseStrategy() {
        return this.reuseStrategy;
    }

    public boolean isDone() {
        return this.responseConsumer.isDone();
    }

}
