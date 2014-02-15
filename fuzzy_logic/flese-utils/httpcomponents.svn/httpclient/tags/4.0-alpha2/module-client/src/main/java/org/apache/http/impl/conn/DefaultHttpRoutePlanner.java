/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha2/module-client/src/main/java/org/apache/http/impl/conn/DefaultHttpRoutePlanner.java $
 * $Revision: 571808 $
 * $Date: 2007-09-01 17:32:48 +0200 (Sat, 01 Sep 2007) $
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

package org.apache.http.impl.conn;

import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.protocol.HttpContext;

import org.apache.http.conn.ClientConnectionManager;
import org.apache.http.conn.HttpRoute;
import org.apache.http.conn.HttpRoutePlanner;
import org.apache.http.conn.Scheme;

import org.apache.http.conn.params.HttpConnParams;


/**
 * Default implementation of an {@link HttpRoutePlanner}.
 * This implementation is based on parameters.
 * It will not make use of any Java system properties.
 */
public class DefaultHttpRoutePlanner implements HttpRoutePlanner {
    
    private ClientConnectionManager connectionManager;
    
    public DefaultHttpRoutePlanner(ClientConnectionManager aConnManager) {
        setConnectionManager(aConnManager);
    }


    // default constructor

    
    public void setConnectionManager(ClientConnectionManager aConnManager) {
        this.connectionManager = aConnManager;
    }


    // non-javadoc, see interface HttpRoutePlanner
    public HttpRoute determineRoute(HttpHost target,
                                    HttpRequest request,
                                    HttpContext context)
        throws HttpException {

        if (target == null) {
            throw new IllegalStateException
                ("Target host must not be null.");
        }
        if (request == null) {
            throw new IllegalStateException
                ("Request must not be null.");
        }

        HttpHost proxy = (HttpHost)
            request.getParams().getParameter(HttpConnParams.DEFAULT_PROXY);

        Scheme schm = this.connectionManager.getSchemeRegistry().
            getScheme(target.getSchemeName());
        // as it is typically used for TLS/SSL, we assume that
        // a layered scheme implies a secure connection
        boolean secure = schm.isLayered();

        HttpRoute route = null;
        if (proxy == null) {
            route = new HttpRoute(target, null, secure);
        } else {
            route = new HttpRoute(target, null, proxy, secure);
        }
        return route;
    }
    
    
}
