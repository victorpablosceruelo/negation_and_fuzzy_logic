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

package org.apache.http.impl.nio.client;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpException;
import org.apache.http.HttpInetConnection;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.nio.ContentDecoder;
import org.apache.http.nio.ContentEncoder;
import org.apache.http.nio.NHttpClientConnection;
import org.apache.http.nio.NHttpClientHandler;
import org.apache.http.nio.NHttpConnection;
import org.apache.http.nio.client.HttpAsyncExchangeHandler;
import org.apache.http.params.CoreProtocolPNames;
import org.apache.http.protocol.HttpContext;

/**
 * Fully asynchronous HTTP client side protocol handler that implements the
 * essential requirements of the HTTP protocol for the server side message
 * processing as described by RFC 2616. It is capable of executing HTTP requests
 * with nearly constant memory footprint. Only HTTP message heads are stored in
 * memory, while content of message bodies is streamed directly from the entity
 * to the underlying channel (and vice versa) using {@link ConsumingNHttpEntity}
 * and {@link ProducingNHttpEntity} interfaces.
 */
class NHttpClientProtocolHandler implements NHttpClientHandler {

    private final Log log = LogFactory.getLog(getClass());

    private static final String HTTP_EXCHNAGE = "http.nio.exchange";

    public NHttpClientProtocolHandler() {
        super();
    }

    private void closeConnection(final NHttpClientConnection conn) {
        try {
            conn.close();
        } catch (IOException ex) {
            try {
                conn.shutdown();
            } catch (IOException ignore) {
                this.log.debug("I/O error terminating connection: " + ex.getMessage(), ex);
            }
        }
    }

    protected void shutdownConnection(final NHttpClientConnection conn) {
        try {
            conn.shutdown();
        } catch (IOException ex) {
            this.log.debug("I/O error terminating connection: " + ex.getMessage(), ex);
        }
    }

    public void connected(final NHttpClientConnection conn, final Object attachment) {
        HttpExchange httpexchange = new HttpExchange();
        HttpContext context = conn.getContext();
        if (this.log.isDebugEnabled()) {
            this.log.debug("Connected " + formatState(conn, httpexchange));
        }
        context.setAttribute(HTTP_EXCHNAGE, httpexchange);
        requestReady(conn);
    }

    public void closed(final NHttpClientConnection conn) {
        HttpContext context = conn.getContext();
        HttpExchange httpexchange = getHttpExchange(context);
        HttpAsyncExchangeHandler<?> handler = getHandler(context);
        if (this.log.isDebugEnabled()) {
            this.log.debug("Disconnected " + formatState(conn, httpexchange));
        }
        if (handler != null) {
            handler.cancel();
        }
    }

    public void exception(final NHttpClientConnection conn, final HttpException ex) {
        HttpContext context = conn.getContext();
        HttpAsyncExchangeHandler<?> handler = getHandler(context);
        this.log.error("HTTP protocol exception: " + ex.getMessage(), ex);
        if (handler != null) {
            handler.failed(ex);
        }
        closeConnection(conn);
    }

    public void exception(final NHttpClientConnection conn, final IOException ex) {
        HttpContext context = conn.getContext();
        HttpAsyncExchangeHandler<?> handler = getHandler(context);
        this.log.error("I/O error: " + ex.getMessage(), ex);
        if (handler != null) {
            handler.failed(ex);
        }
        shutdownConnection(conn);
    }

    public void requestReady(final NHttpClientConnection conn) {
        HttpContext context = conn.getContext();
        HttpExchange httpexchange = getHttpExchange(context);
        HttpAsyncExchangeHandler<?> handler = getHandler(context);
        if (this.log.isDebugEnabled()) {
            this.log.debug("Request ready " + formatState(conn, httpexchange));
        }
        if (httpexchange.getRequestState() != MessageState.READY) {
            return;
        }
        if (handler == null || handler.isDone()) {
            if (this.log.isDebugEnabled()) {
                this.log.debug("No request submitted " + formatState(conn, httpexchange));
            }
            return;
        }
        try {
            HttpRequest request = handler.generateRequest();
            httpexchange.setRequest(request);

            HttpEntityEnclosingRequest entityReq = null;
            if (request instanceof HttpEntityEnclosingRequest) {
                entityReq = (HttpEntityEnclosingRequest) request;
            }

            conn.submitRequest(request);

            if (entityReq != null) {
                if (entityReq.expectContinue()) {
                    int timeout = conn.getSocketTimeout();
                    httpexchange.setTimeout(timeout);
                    timeout = request.getParams().getIntParameter(
                            CoreProtocolPNames.WAIT_FOR_CONTINUE, 3000);
                    conn.setSocketTimeout(timeout);
                    httpexchange.setRequestState(MessageState.ACK);
                } else {
                    httpexchange.setRequestState(MessageState.BODY_STREAM);
                }
            } else {
                httpexchange.setRequestState(MessageState.COMPLETED);
            }
        } catch (IOException ex) {
            if (this.log.isDebugEnabled()) {
                this.log.debug("I/O error: " + ex.getMessage(), ex);
            }
            shutdownConnection(conn);
            handler.failed(ex);
        } catch (HttpException ex) {
            if (this.log.isDebugEnabled()) {
                this.log.debug("HTTP protocol exception: " + ex.getMessage(), ex);
            }
            closeConnection(conn);
            handler.failed(ex);
        }
    }

    public void inputReady(final NHttpClientConnection conn, final ContentDecoder decoder) {
        HttpContext context = conn.getContext();
        HttpExchange httpexchange = getHttpExchange(context);
        HttpAsyncExchangeHandler<?> handler = getHandler(context);
        if (this.log.isDebugEnabled()) {
            this.log.debug("Input ready " + formatState(conn, httpexchange));
        }
        try {
            handler.consumeContent(decoder, conn);
            if (decoder.isCompleted()) {
                processResponse(conn, httpexchange, handler);
            }
        } catch (IOException ex) {
            if (this.log.isDebugEnabled()) {
                this.log.debug("I/O error: " + ex.getMessage(), ex);
            }
            shutdownConnection(conn);
            handler.failed(ex);
        }
    }

    public void outputReady(final NHttpClientConnection conn, final ContentEncoder encoder) {
        HttpContext context = conn.getContext();
        HttpExchange httpexchange = getHttpExchange(context);
        HttpAsyncExchangeHandler<?> handler = getHandler(context);
        if (this.log.isDebugEnabled()) {
            this.log.debug("Output ready " + formatState(conn, httpexchange));
        }
        try {
            if (httpexchange.getRequestState() == MessageState.ACK) {
                conn.suspendOutput();
                return;
            }
            handler.produceContent(encoder, conn);
            if (encoder.isCompleted()) {
                httpexchange.setRequestState(MessageState.COMPLETED);
            }
        } catch (IOException ex) {
            if (this.log.isDebugEnabled()) {
                this.log.debug("I/O error: " + ex.getMessage(), ex);
            }
            shutdownConnection(conn);
            handler.failed(ex);
        }
    }

    public void responseReceived(final NHttpClientConnection conn) {
        HttpContext context = conn.getContext();
        HttpExchange httpexchange = getHttpExchange(context);
        HttpAsyncExchangeHandler<?> handler = getHandler(context);
        if (this.log.isDebugEnabled()) {
            this.log.debug("Response received " + formatState(conn, httpexchange));
        }
        try {
            HttpResponse response = conn.getHttpResponse();
            HttpRequest request = httpexchange.getRequest();

            int statusCode = response.getStatusLine().getStatusCode();
            if (statusCode < HttpStatus.SC_OK) {
                // 1xx intermediate response
                if (statusCode == HttpStatus.SC_CONTINUE
                        && httpexchange.getRequestState() == MessageState.ACK) {
                    continueRequest(conn, httpexchange);
                    httpexchange.setRequestState(MessageState.BODY_STREAM);
                }
                return;
            } else {
                httpexchange.setResponse(response);
                if (httpexchange.getRequestState() == MessageState.ACK) {
                    cancelRequest(conn, httpexchange);
                    httpexchange.setRequestState(MessageState.COMPLETED);
                } else if (httpexchange.getRequestState() == MessageState.BODY_STREAM) {
                    // Early response
                    cancelRequest(conn, httpexchange);
                    httpexchange.invalidate();
                    conn.suspendOutput();
                }
            }
            handler.responseReceived(response);
            if (!canResponseHaveBody(request, response)) {
                processResponse(conn, httpexchange, handler);
            }
        } catch (IOException ex) {
            if (this.log.isDebugEnabled()) {
                this.log.debug("I/O error: " + ex.getMessage(), ex);
            }
            shutdownConnection(conn);
            handler.failed(ex);
        } catch (HttpException ex) {
            if (this.log.isDebugEnabled()) {
                this.log.debug("HTTP protocol exception: " + ex.getMessage(), ex);
            }
            closeConnection(conn);
            handler.failed(ex);
        }
    }

    public void timeout(final NHttpClientConnection conn) {
        HttpContext context = conn.getContext();
        HttpExchange httpexchange = getHttpExchange(context);
        HttpAsyncExchangeHandler<?> handler = getHandler(context);
        if (this.log.isDebugEnabled()) {
            this.log.debug("Timeout " + formatState(conn, httpexchange));
        }
        try {
            if (httpexchange.getRequestState() == MessageState.ACK) {
                continueRequest(conn, httpexchange);
                httpexchange.setRequestState(MessageState.BODY_STREAM);
            } else {
                if (conn.getStatus() == NHttpConnection.ACTIVE) {
                    conn.close();
                    if (conn.getStatus() == NHttpConnection.CLOSING) {
                        // Give the connection some grace time to
                        // close itself nicely
                        conn.setSocketTimeout(250);
                    }
                } else {
                    conn.shutdown();
                }
            }
        } catch (IOException ex) {
            if (this.log.isDebugEnabled()) {
                this.log.debug("I/O error: " + ex.getMessage(), ex);
            }
            shutdownConnection(conn);
            handler.failed(ex);
        }
    }

    private HttpExchange getHttpExchange(final HttpContext context) {
        return (HttpExchange) context.getAttribute(HTTP_EXCHNAGE);
    }

    private HttpAsyncExchangeHandler<?> getHandler(final HttpContext context) {
        return (HttpAsyncExchangeHandler<?>) context.getAttribute(
                DefaultAsyncRequestDirector.HTTP_EXCHANGE_HANDLER);
    }

    private void continueRequest(
            final NHttpClientConnection conn,
            final HttpExchange httpexchange) {
        int timeout = httpexchange.getTimeout();
        conn.setSocketTimeout(timeout);
        conn.requestOutput();
    }

    private void cancelRequest(
            final NHttpClientConnection conn,
            final HttpExchange httpexchange) throws IOException {
        int timeout = httpexchange.getTimeout();
        conn.setSocketTimeout(timeout);
        conn.resetOutput();
        httpexchange.resetOutput();
    }

    private void processResponse(
            final NHttpClientConnection conn,
            final HttpExchange httpexchange,
            final HttpAsyncExchangeHandler<?> handler) throws IOException {
        if (!httpexchange.isValid()) {
            conn.close();
        }
        HttpRequest request = httpexchange.getRequest();
        HttpResponse response = httpexchange.getResponse();

        String method = request.getRequestLine().getMethod();
        int status = response.getStatusLine().getStatusCode();
        if (method.equalsIgnoreCase("CONNECT") && status == HttpStatus.SC_OK) {
            this.log.debug("CONNECT method succeeded");
            conn.resetInput();
        } else {
            if (!handler.keepAlive(response)) {
                conn.close();
            }
        }
        if (this.log.isDebugEnabled()) {
            this.log.debug("Response processed " + formatState(conn, httpexchange));
        }
        handler.responseCompleted();
        httpexchange.reset();
    }

    private boolean canResponseHaveBody(final HttpRequest request, final HttpResponse response) {

        String method = request.getRequestLine().getMethod();
        int status = response.getStatusLine().getStatusCode();

        if (method.equalsIgnoreCase("HEAD")) {
            return false;
        }
        if (method.equalsIgnoreCase("CONNECT") && status == HttpStatus.SC_OK) {
            return false;
        }
        return status >= HttpStatus.SC_OK
            && status != HttpStatus.SC_NO_CONTENT
            && status != HttpStatus.SC_NOT_MODIFIED
            && status != HttpStatus.SC_RESET_CONTENT;
    }

    private String formatState(final NHttpConnection conn, final HttpExchange httpexchange) {
        StringBuilder buf = new StringBuilder();
        buf.append("[");
        if (conn.isOpen() && (conn instanceof HttpInetConnection)) {
            HttpInetConnection inetconn = (HttpInetConnection) conn;
            buf.append(inetconn.getRemoteAddress());
            buf.append(":");
            buf.append(inetconn.getRemotePort());
        }
        buf.append("(");
        buf.append(conn.isOpen() ? "open" : "closed");
        buf.append("),request=");
        buf.append(httpexchange.getRequestState());
        if (httpexchange.getRequest() != null) {
            buf.append("(");
            buf.append(httpexchange.getRequest().getRequestLine());
            buf.append(")");
        }
        buf.append(",response=");
        buf.append(httpexchange.getResponseState());
        if (httpexchange.getResponse() != null) {
            buf.append("(");
            buf.append(httpexchange.getResponse().getStatusLine());
            buf.append(")");
        }
        buf.append(",valid=");
        buf.append(httpexchange.isValid());
        buf.append("]");
        return buf.toString();
    }

}
