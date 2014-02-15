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
import java.io.InputStream;
import java.net.URI;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.config.ConnectionConfig;
import org.apache.http.config.SocketConfig;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.util.VersionInfo;

public class ApacheHttpClient implements HttpAgent {

    private final PoolingHttpClientConnectionManager mgr;
    private final CloseableHttpClient httpclient;

    public ApacheHttpClient() {
        super();
        final ConnectionConfig connectionConfig = ConnectionConfig.custom()
                .setBufferSize(8 * 1024)
                .setFragmentSizeHint(8 * 1024)
                .build();
        final SocketConfig socketConfig = SocketConfig.custom()
                .setSoTimeout(15000)
                .build();
        this.mgr = new PoolingHttpClientConnectionManager();
        this.mgr.setDefaultSocketConfig(socketConfig);
        this.mgr.setDefaultConnectionConfig(connectionConfig);
        this.httpclient = HttpClients.createMinimal(this.mgr);
    }

    @Override
    public void init() {
    }

    @Override
    public void shutdown() {
        this.mgr.shutdown();
    }

    Stats execute(final URI target, final byte[] content, final int n, final int c) throws Exception {
        this.mgr.setMaxTotal(2000);
        this.mgr.setDefaultMaxPerRoute(c);
        final Stats stats = new Stats(n, c);
        final WorkerThread[] workers = new WorkerThread[c];
        for (int i = 0; i < workers.length; i++) {
            workers[i] = new WorkerThread(stats, target, content);
        }
        for (final WorkerThread worker : workers) {
            worker.start();
        }
        for (final WorkerThread worker : workers) {
            worker.join();
        }
        return stats;
    }

    class WorkerThread extends Thread {

        private final Stats stats;
        private final URI target;
        private final byte[] content;

        WorkerThread(final Stats stats, final URI target, final byte[] content) {
            super();
            this.stats = stats;
            this.target = target;
            this.content = content;
        }

        @Override
        public void run() {
            final byte[] buffer = new byte[4096];
            while (!this.stats.isComplete()) {
                HttpUriRequest request;
                if (this.content == null) {
                    final HttpGet httpget = new HttpGet(target);
                    request = httpget;
                } else {
                    final HttpPost httppost = new HttpPost(target);
                    httppost.setEntity(new ByteArrayEntity(content));
                    request = httppost;
                }
                long contentLen = 0;
                try {
                    final HttpResponse response = httpclient.execute(request);
                    final HttpEntity entity = response.getEntity();
                    if (entity != null) {
                        final InputStream instream = entity.getContent();
                        contentLen = 0;
                        if (instream != null) {
                            try {
                                int l = 0;
                                while ((l = instream.read(buffer)) != -1) {
                                    contentLen += l;
                                }
                            } finally {
                                instream.close();
                            }
                        }
                    }
                    if (response.getStatusLine().getStatusCode() == 200) {
                        this.stats.success(contentLen);
                    } else {
                        this.stats.failure(contentLen);
                    }
                } catch (final IOException ex) {
                    this.stats.failure(contentLen);
                    request.abort();
                }
            }
        }

    }

    @Override
    public Stats get(final URI target, final int n, final int c) throws Exception {
        return execute(target, null, n ,c);
    }

    @Override
    public Stats post(final URI target, final byte[] content, final int n, final int c) throws Exception {
        return execute(target, content, n, c);
    }

    @Override
    public String getClientName() {
        final VersionInfo vinfo = VersionInfo.loadVersionInfo("org.apache.http.client",
                Thread.currentThread().getContextClassLoader());
        return "Apache HttpClient (ver: " +
            ((vinfo != null) ? vinfo.getRelease() : VersionInfo.UNAVAILABLE) + ")";
    }

    public static void main(final String[] args) throws Exception {
        final Config config = BenchRunner.parseConfig(args);
        BenchRunner.run(new ApacheHttpClient(), config);
    }

}