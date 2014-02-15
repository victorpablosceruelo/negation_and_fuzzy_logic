/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/AsyncHttpProcessor.java $
 * $Revision: 489367 $
 * $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
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

package org.apache.http.async;

import java.io.IOException;

import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.ProtocolException;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;
import org.apache.http.protocol.HttpProcessor;
import org.apache.http.protocol.HttpRequestExecutor;

/**
 * HTTP processor for asynchronously dispatched requests.
 * This is the asynchronous version of
 * {@link org.apache.http.protocol.HttpRequestExecutor HttpRequestExecutor}.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public class AsyncHttpProcessor extends HttpRequestExecutor {
    
    /**
     * Create a new async HTTP processor.
     *
     * @param proc      the underlying HTTP processor to use
     */
    public AsyncHttpProcessor(HttpProcessor proc) {
        super(proc);
    }


    // non-javadoc, see base class HttpRequestExecutor
    // make protected method accessible in this package, too
    protected void doPrepareRequest(final HttpRequest request,
                                    final HttpContext context)
        throws HttpException, IOException {
        super.doPrepareRequest(request, context);
    }

    /**
     * Establish a connection with the target host.
     *
     * @param conn      the HTTP connection
     * @param target    the target host for the request, or
     *                  <code>null</code> to send to the host already
     *                  set as the connection target
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected void doEstablishConnection(
            final HttpAsyncClientConnection conn, 
            final HttpHost target,
            final HttpParams params) throws HttpException, IOException {
        if (conn == null) {
            throw new IllegalArgumentException("HTTP connection may not be null");
        }
        if (target == null) {
            throw new IllegalArgumentException("Target host may not be null");
        }
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        // make sure the connection is open and points to the target host
        if ((target == null) || target.equals(conn.getTargetHost())) {
            // host and port ok, check whether connection needs to be opened
            if (HttpConnectionParams.isStaleCheckingEnabled(params)) {
                if (conn.isOpen() && conn.isStale()) {
                    conn.close();
                }
            }
            if (!conn.isOpen()) {
                conn.open(params);
                // TODO: Implement secure tunnelling (@@@ HttpRequestExecutor)
            }

        } else {
            // wrong target, point connection to target
            if (conn.isOpen()) {
                conn.close();
            }
            conn.setTargetHost(target);
            conn.open(params);

        } // if connection points to target else
    }
    
    /**
     * Establish a connection and send a request.
     *
     * @param request     the request to send, already prepared
     * @param connection  the connection over which to send the request
     * @param context     the context for the request
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected void asyncSendRequest(HttpRequest               request,
                                    HttpAsyncClientConnection connection,
                                    HttpContext               context)
        throws HttpException, IOException {

        if (request == null)
            throw new IllegalArgumentException("request must not be null");
        if (connection == null)
            throw new IllegalArgumentException("connection must not be null");
        if (context == null)
            throw new IllegalArgumentException("context must not be null");

        HttpHost target = (HttpHost)
            context.getAttribute(HttpExecutionContext.HTTP_TARGET_HOST);
        if (target == null) {
            throw new IllegalStateException
                ("target host missing in request context");
        }
        if (request instanceof HttpEntityEnclosingRequest 
            && ((HttpEntityEnclosingRequest)request).expectContinue()) {
            throw new ProtocolException
                ("Expect-continue handshake not supported"); 
        }
        doEstablishConnection(connection, target, request.getParams());
        
        context.setAttribute(HttpExecutionContext.HTTP_CONNECTION, 
                             connection);
        
        super.doSendRequest(request, connection, context);

    } // asyncSendRequest


    /**
     * Wait for and receive a response.
     *
     * @param request     the request for which to obtain the response
     * @param connection  the connection over which the request was sent
     * @param context     the context for the matching request
     *
     * @return  the response, not yet post-processed
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected
        HttpResponse doReceiveResponse(HttpRequest               request,
                                       HttpAsyncClientConnection connection,
                                       HttpContext               context)
        throws HttpException, IOException {

        // argument checking is done here...
        return super.doReceiveResponse(request, connection, context);

    } // doReceiveResponse


    /**
     * Finish a response.
     * This includes post-processing of the response object.
     * It does <i>not</i> read the response entity (if any), nor allows
     * re-use of the connection over which the response is coming in.
     *
     * @param response    the response object to finish
     * @param context     the context for the matching request
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected void doFinishResponse(HttpResponse response,
                                    HttpContext context)
        throws HttpException, IOException {

        // argument checking is done here...
        super.doFinishResponse(response, context);

    } // doFinishResponse


} // class AsyncHttpProcessor
