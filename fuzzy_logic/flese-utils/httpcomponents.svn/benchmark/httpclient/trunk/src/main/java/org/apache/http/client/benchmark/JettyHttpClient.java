/*
 * ====================================================================
 *
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
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
package org.apache.http.client.benchmark;

import java.io.IOException;
import java.net.URI;
import java.util.concurrent.Semaphore;

import org.eclipse.jetty.client.HttpClient;
import org.eclipse.jetty.client.HttpExchange;
import org.eclipse.jetty.io.Buffer;
import org.eclipse.jetty.io.ByteArrayBuffer;
import org.eclipse.jetty.server.Server;

public class JettyHttpClient implements HttpAgent {

    private final HttpClient client;

    public JettyHttpClient() {
        super();
        this.client = new HttpClient();
        this.client.setRequestBufferSize(8 * 1024);
        this.client.setResponseBufferSize(8 * 1024);
        this.client.setConnectorType(HttpClient.CONNECTOR_SELECT_CHANNEL);
        this.client.setTimeout(15000);
    }

    @Override
    public void init() throws Exception {
        this.client.start();
    }

    @Override
    public void shutdown() throws Exception {
        this.client.stop();
    }

    Stats execute(final URI targetURI, final byte[] content, final int n, final int c) throws Exception {
        this.client.setMaxConnectionsPerAddress(c);
        final Stats stats = new Stats(n, c);
        final Semaphore semaphore = new Semaphore(c);
        for (int i = 0; i < n; i++) {
            semaphore.acquire();
            final SimpleHttpExchange exchange = new SimpleHttpExchange(stats, semaphore);
            exchange.setURL(targetURI.toASCIIString());

            if (content != null) {
                exchange.setMethod("POST");
                exchange.setRequestContent(new ByteArrayBuffer(content));
            }
            try {
                this.client.send(exchange);
            } catch (final IOException ex) {
                semaphore.release();
                stats.failure(0L);
            }
        }
        stats.waitFor();
        return stats;
    }

    @Override
    public Stats get(final URI target, final int n, final int c) throws Exception {
        return execute(target, null, n, c);
    }

    @Override
    public Stats post(final URI target, final byte[] content, final int n, final int c) throws Exception {
        return execute(target, content, n, c);
    }

    @Override
    public String getClientName() {
        return "Jetty " + Server.getVersion();
    }

    static class SimpleHttpExchange extends HttpExchange {

        private final Stats stats;
        private final Semaphore semaphore;

        private int status = 0;
        private long contentLen = 0;

        SimpleHttpExchange(final Stats stats, final Semaphore semaphore) {
            super();
            this.stats = stats;
            this.semaphore = semaphore;
        }

        @Override
        protected void onResponseStatus(
                final Buffer version, final int status, final Buffer reason) throws IOException {
            this.status = status;
            super.onResponseStatus(version, status, reason);
        }

        @Override
        protected void onResponseContent(final Buffer content) throws IOException {
            final byte[] tmp = new byte[content.length()];
            content.get(tmp, 0, tmp.length);
            this.contentLen += tmp.length;
            super.onResponseContent(content);
        }

        @Override
        protected void onResponseComplete() throws IOException {
            this.semaphore.release();
            if (this.status == 200) {
                this.stats.success(this.contentLen);
            } else {
                this.stats.failure(this.contentLen);
            }
            super.onResponseComplete();
        }

        @Override
        protected void onConnectionFailed(final Throwable x) {
            this.semaphore.release();
            this.stats.failure(this.contentLen);
            super.onConnectionFailed(x);
        }

        @Override
        protected void onException(final Throwable x) {
            this.semaphore.release();
            this.stats.failure(this.contentLen);
            super.onException(x);
        }

    }

    public static void main(final String[] args) throws Exception {
        final Config config = BenchRunner.parseConfig(args);
        BenchRunner.run(new JettyHttpClient(), config);
    }

}