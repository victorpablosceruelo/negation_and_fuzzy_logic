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
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;

public class HttpJRE implements HttpAgent {

    public HttpJRE() {
        super();
    }

    @Override
    public void init() {
    }

    @Override
    public void shutdown() {
    }

    Stats execute(final URI targetURI, final byte[] content, final int n, final int c) throws Exception {
        System.setProperty("http.maxConnections", Integer.toString(c));
        final URL target = targetURI.toURL();
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
        private final URL target;
        private final byte[] content;

        WorkerThread(final Stats stats, final URL target, final byte[] content) {
            super();
            this.stats = stats;
            this.target = target;
            this.content = content;
        }

        @Override
        public void run() {
            final byte[] buffer = new byte[4096];
            while (!this.stats.isComplete()) {
                long contentLen = 0;
                try {
                    final HttpURLConnection conn = (HttpURLConnection) this.target.openConnection();
                    conn.setReadTimeout(15000);

                    if (this.content != null) {
                        conn.setRequestMethod("POST");
                        conn.setFixedLengthStreamingMode(this.content.length);
                        conn.setUseCaches (false);
                        conn.setDoInput(true);
                        conn.setDoOutput(true);
                        final OutputStream out = conn.getOutputStream();
                        try {
                            out.write(this.content);
                            out.flush ();
                        } finally {
                            out.close();
                        }
                    }
                    final InputStream instream = conn.getInputStream();
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
                    if (conn.getResponseCode() == 200) {
                        this.stats.success(contentLen);
                    } else {
                        this.stats.failure(contentLen);
                    }
                } catch (final IOException ex) {
                    this.stats.failure(contentLen);
                }
            }
        }
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
        return "JRE HTTP " + System.getProperty("java.version");
    }

    public static void main(final String[] args) throws Exception {
        final Config config = BenchRunner.parseConfig(args);
        BenchRunner.run(new HttpJRE(), config);
    }

}