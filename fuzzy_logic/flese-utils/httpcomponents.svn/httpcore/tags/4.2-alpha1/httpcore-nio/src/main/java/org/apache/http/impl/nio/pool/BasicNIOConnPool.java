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
package org.apache.http.impl.nio.pool;

import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.http.HttpHost;
import org.apache.http.annotation.ThreadSafe;
import org.apache.http.concurrent.FutureCallback;
import org.apache.http.nio.pool.AbstractNIOConnPool;
import org.apache.http.nio.reactor.ConnectingIOReactor;
import org.apache.http.nio.reactor.IOSession;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;

/**
 * Basic non-blocking {@link IOSession} pool.
 * <p>
 * The following parameters can be used to customize the behavior of this
 * class:
 * <ul>
 *  <li>{@link org.apache.http.params.CoreConnectionPNames#CONNECTION_TIMEOUT}</li>
 * </ul>
 *
 * @since 4.2
 */
@ThreadSafe
public class BasicNIOConnPool extends AbstractNIOConnPool<HttpHost, IOSession, BasicNIOPoolEntry> {

    private static AtomicLong COUNTER = new AtomicLong();

    private final HttpParams params;

    public BasicNIOConnPool(final ConnectingIOReactor ioreactor, final HttpParams params) {
        super(ioreactor, 2, 20);
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        this.params = params;
    }

    @Override
    protected SocketAddress resolveRemoteAddress(final HttpHost host) {
        return new InetSocketAddress(host.getHostName(), host.getPort());
    }

    @Override
    protected SocketAddress resolveLocalAddress(final HttpHost host) {
        return null;
    }

    @Override
    protected IOSession createConnection(final HttpHost route, final IOSession session) {
        return session;
    }

    @Override
    protected BasicNIOPoolEntry createEntry(final HttpHost host, final IOSession conn) {
        return new BasicNIOPoolEntry(Long.toString(COUNTER.getAndIncrement()), host, conn);
    }

    @Override
    protected void closeEntry(final BasicNIOPoolEntry entry) {
        IOSession iosession = entry.getConnection();
        iosession.shutdown();
    }

    @Override
    public Future<BasicNIOPoolEntry> lease(
            final HttpHost route,
            final Object state,
            final FutureCallback<BasicNIOPoolEntry> callback) {
        int connectTimeout = HttpConnectionParams.getConnectionTimeout(this.params);
        return super.lease(route, state, connectTimeout, TimeUnit.MILLISECONDS, callback);
    }

    @Override
    public Future<BasicNIOPoolEntry> lease(final HttpHost route, final Object state) {
        int connectTimeout = HttpConnectionParams.getConnectionTimeout(this.params);
        return super.lease(route, state, connectTimeout, TimeUnit.MILLISECONDS, null);
    }

}
