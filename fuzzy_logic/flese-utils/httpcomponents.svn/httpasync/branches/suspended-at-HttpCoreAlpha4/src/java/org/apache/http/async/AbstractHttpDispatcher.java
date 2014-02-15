/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/AbstractHttpDispatcher.java $
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
import java.util.Collection;
import java.util.LinkedList;

import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;

/**
 * Abstract base for implementations of {@link HttpDispatcher HttpDispatcher}.
 * Provides access to protected methods in
 * {@link AsyncHttpProcessor AsyncHttpProcessor}.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public abstract class AbstractHttpDispatcher implements HttpDispatcher {


    /**
     * Context attribute indicating whether a request is preprocessed.
     * Set to a {@link Boolean Boolean} <code>true</code> when preprocessed.
     * <!-- @@@ find a better place for this definition -->
     */
    public final static String CTXT_REQUEST_PREPROCESSED =
        "http.request.preprocessed";


    /**
     * All handles linked to this dispatcher.
     * This is assumed to be used as a set, but declared a collection to
     * avoid the overhead of duplicate checking. A set implementation can
     * be passed to the constructor if necessary.
     */
    protected final Collection linked_handles;

    /** The monitor object for accessing the set of linked handles. */
    protected final Object linked_handle_monitor =
        new String("AbstractHttpDispatcher.linked_handle_monitor");


    /**
     * Initialize this new (abstract) dispatcher.
     *
     * @param linked    the initial linked handles, or
     *                  <code>null</code> to create an empty initial set.
     *                  This can also be used to choose the implementation
     *                  of {@link Collection Collection}.
     */
    protected AbstractHttpDispatcher(Collection linked) {

        linked_handles = (linked != null) ? linked : new LinkedList();

    } // constructor



    /**
     * Perform postprocessing on a response.
     * After postprocessing, the response is ready for the application.
     * This method is meant to be called from handles, to perform
     * postprocessing in an application thread.
     *
     * @param handle      the calling handle
     * @param response    the response object to postprocess
     *    Note that {@link HttpHandle#getResponse handle.getResponse()}
     *    can not be used here, since it could call this method!
     */
    protected abstract void postprocessResponse(AbstractHttpHandle handle,
                                                HttpResponse response)
        throws HttpException, IOException
        ;


    /**
     * Close a handle.
     *
     * @param handle    the handle to close
     * @param abort     <code>false</code> if this is a smooth
     *                  {@link HttpHandle#close close},
     *                  <code>true</code> if it is a hard
     *                  {@link HttpHandle#abort abort}
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected abstract void closeHandle(AbstractHttpHandle handle,
                                        boolean abort)
        throws HttpException, IOException
        ;


    /**
     * Prepares a new request specific context.
     *
     * @param proc        the processor to be used for executing the request
     * @param target      the target host for the request
     * @param context     the parent context for sending the request,
     *                    or <code>null</code> to use the default context
     *
     * @return    a new context specific to the request
     *
     * @throws HttpException      if the request can not be prepared
     */
    protected static HttpContext prepareContext(AsyncHttpProcessor proc,
                                                HttpHost           target,
                                                HttpContext        context)
        throws HttpException {

        if (proc == null)
            throw new IllegalArgumentException
                ("HTTP processor must not be null.");
        if (target == null)
            throw new IllegalArgumentException
                ("Target host must not be null.");

        HttpExecutionContext hxc = new HttpExecutionContext(context);
        hxc.setAttribute(HttpExecutionContext.HTTP_TARGET_HOST, target);

        return hxc;

    } // prepareContext


    /**
     * Prepares a request for sending.
     * This performs preprocessing on the request.
     *
     * @param proc        the processor to use for preparing
     * @param request     the request to prepare
     * @param context     the request specific context
     *
     * @throws HttpException      if the request can not be prepared
     * @throws IOException        in case of an IO problem
     */
    protected static void prepareRequest(AsyncHttpProcessor proc,
                                         HttpRequest        request,
                                         HttpContext        context)
        throws HttpException, IOException {

        if (proc == null)
            throw new IllegalArgumentException
                ("HTTP processor must not be null.");
        if (request == null)
            throw new IllegalArgumentException
                ("Request must not be null.");
        if (context == null)
            throw new IllegalArgumentException
                ("Context must not be null.");

        if (Boolean.TRUE
            .equals(context.getAttribute(CTXT_REQUEST_PREPROCESSED)))
            throw new IllegalArgumentException
                ("Request already preprocessed.");

        context.setAttribute(HttpExecutionContext.HTTP_REQUEST, request);
        // set the flag before preprocessing, in case we get an exception
        context.setAttribute(CTXT_REQUEST_PREPROCESSED, Boolean.TRUE);

        proc.doPrepareRequest(request, context);

    } // prepareRequest


    /**
     * Establishe a connection and send a request.
     * Maps to
     * {@link AsyncHttpProcessor#asyncSendRequest
     *        proc.asyncSendRequest}.
     *
     * @param proc        the processor to use for transmission
     * @param request     the request to send, already prepared
     * @param context     the request specific context <i>returned</i> by
     *                    {@link #prepareRequest prepareRequest}
     *                    when the request was prepared
     * @param connection  the connection over which to send the request
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected static void transmitRequestTo(AsyncHttpProcessor        proc,
                                            HttpRequest               request,
                                            HttpContext               context,
                                            HttpAsyncClientConnection connection)
        throws HttpException, IOException {

        proc.asyncSendRequest(request, connection, context);

    } // transmitRequestTo


    /**
     * Wait for and receive a response.
     * Maps to
     * {@link AsyncHttpProcessor#doReceiveResponse proc.doReceiveResponse}.
     *
     * @param proc        the processor to use for transmission
     * @param request     the request for which to obtain the response
     * @param connection  the connection over which the request was sent
     *
     * @return  the response, not yet post-processed
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected static
        HttpResponse obtainResponse(AsyncHttpProcessor        proc,
                                    HttpRequest               request,
                                    HttpContext               context,
                                    HttpAsyncClientConnection connection)
        throws HttpException, IOException {

        return proc.doReceiveResponse(request, connection, context);

    } // obtainResponse


    /**
     * Finish a response.
     * Maps to
     * {@link AsyncHttpProcessor#doFinishResponse proc.doFinishResponse}.
     *
     * @param response    the response object to finish
     * @param context     the context obtained from
     *                    {@link #prepareRequest prepareRequest}
     *                    for the matching request
     *
     * @throws HttpException      in case of a problem
     * @throws IOException        in case of an IO problem
     */
    protected static void finishResponse(AsyncHttpProcessor  proc,
                                         HttpResponse        response,
                                         HttpContext         context)
        throws HttpException, IOException {

        proc.doFinishResponse(response, context);

    } // finishResponse


} // class AbstractHttpDispatcher
