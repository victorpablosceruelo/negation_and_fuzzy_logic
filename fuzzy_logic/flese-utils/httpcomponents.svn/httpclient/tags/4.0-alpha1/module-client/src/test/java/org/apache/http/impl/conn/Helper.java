/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/test/java/org/apache/http/impl/conn/Helper.java $
 * $Revision: 542225 $
 * $Date: 2007-05-28 15:34:30 +0200 (Mon, 28 May 2007) $
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

import org.apache.http.HttpClientConnection;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpParamsLinker;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;
import org.apache.http.protocol.HttpProcessor;
import org.apache.http.protocol.HttpRequestExecutor;


/**
 * Static helper methods.
 */
public final class Helper {

    /** Disabled default constructor. */
    private Helper() {
        // no body
    }


    /**
     * Executes a request.
     */
    public static HttpResponse execute(HttpRequest req,
                                       HttpClientConnection conn,
                                       HttpHost target,
                                       HttpRequestExecutor exec,
                                       HttpProcessor proc,
                                       HttpParams params,
                                       HttpContext ctxt)
        throws Exception {

        ctxt.setAttribute(HttpExecutionContext.HTTP_CONNECTION, conn);
        ctxt.setAttribute(HttpExecutionContext.HTTP_TARGET_HOST, target);
        ctxt.setAttribute(HttpExecutionContext.HTTP_REQUEST, req);

        HttpParamsLinker.link(req, params);
        exec.preProcess(req, proc, ctxt);
        HttpResponse rsp = exec.execute(req, conn, ctxt);
        HttpParamsLinker.link(rsp, params);
        exec.postProcess(rsp, proc, ctxt);

        return rsp;
    }

}
