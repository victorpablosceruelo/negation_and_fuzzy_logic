/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/test/java/org/apache/http/impl/conn/ClientConnAdapterMockup.java $
 * $Revision: 540118 $
 * $Date: 2007-05-21 13:23:57 +0200 (Mon, 21 May 2007) $
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

package org.apache.http.impl.conn;

import org.apache.http.conn.HttpRoute;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HttpContext;


/**
 * Mockup connection adapter.
 */
public class ClientConnAdapterMockup extends AbstractClientConnAdapter {

    public ClientConnAdapterMockup() {
        super(null, null);
    }

    public void close() {
        throw new UnsupportedOperationException("just a mockup");
    }

    public HttpRoute getRoute() {
        throw new UnsupportedOperationException("just a mockup");
    }

    public void layerProtocol(HttpContext context, HttpParams params) {
        throw new UnsupportedOperationException("just a mockup");
    }

    public void open(HttpRoute route, HttpContext context, HttpParams params) {
        throw new UnsupportedOperationException("just a mockup");
    }

    public void shutdown() {
        throw new UnsupportedOperationException("just a mockup");
    }

    public void tunnelCreated(boolean secure, HttpParams params) {
        throw new UnsupportedOperationException("just a mockup");
    }

}
