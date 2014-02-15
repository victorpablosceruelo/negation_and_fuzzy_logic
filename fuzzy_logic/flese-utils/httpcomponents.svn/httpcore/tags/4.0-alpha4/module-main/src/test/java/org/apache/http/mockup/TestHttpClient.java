/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-main/src/test/java/org/apache/http/mockup/TestHttpClient.java $
 * $Revision: 510645 $
 * $Date: 2007-02-22 21:30:13 +0100 (Thu, 22 Feb 2007) $
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

package org.apache.http.mockup;

import java.io.IOException;

import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpClientConnection;
import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpVersion;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.protocol.BasicHttpProcessor;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;
import org.apache.http.protocol.HttpRequestExecutor;
import org.apache.http.protocol.RequestConnControl;
import org.apache.http.protocol.RequestContent;
import org.apache.http.protocol.RequestExpectContinue;
import org.apache.http.protocol.RequestTargetHost;
import org.apache.http.protocol.RequestUserAgent;

public class TestHttpClient {

    private final HttpParams params;
    private final BasicHttpProcessor httpproc;
    private final HttpRequestExecutor httpexecutor;
    private final ConnectionReuseStrategy connStrategy;
    private final HttpContext context;
    
    public TestHttpClient() {
        super();
        this.params = new BasicHttpParams(null);
        this.params
            .setIntParameter(HttpConnectionParams.SO_TIMEOUT, 5000)
            .setBooleanParameter(HttpConnectionParams.STALE_CONNECTION_CHECK, false)
            .setParameter(HttpProtocolParams.PROTOCOL_VERSION, HttpVersion.HTTP_1_1)
            .setParameter(HttpProtocolParams.USER_AGENT, "TEST-CLIENT/1.1");

        this.httpproc = new BasicHttpProcessor();
        // Required protocol interceptors
        this.httpproc.addInterceptor(new RequestContent());
        this.httpproc.addInterceptor(new RequestTargetHost());
        // Recommended protocol interceptors
        this.httpproc.addInterceptor(new RequestConnControl());
        this.httpproc.addInterceptor(new RequestUserAgent());
        this.httpproc.addInterceptor(new RequestExpectContinue());

        this.httpexecutor = new HttpRequestExecutor(this.httpproc);
        this.httpexecutor.setParams(this.params);
        this.connStrategy = new DefaultConnectionReuseStrategy();
        this.context = new HttpExecutionContext(null);
    }

    public HttpParams getParams() {
        return this.params;
    }
    
    public HttpResponse execute(
            final HttpRequest request,
            final HttpHost targetHost,
            final HttpClientConnection conn) throws HttpException, IOException {
        this.context.setAttribute(HttpExecutionContext.HTTP_TARGET_HOST, targetHost);
        return this.httpexecutor.execute(request, conn, this.context);
    }
    
    public boolean keepAlive(final HttpResponse response) {
        return this.connStrategy.keepAlive(response, this.context);
    }
    
}
