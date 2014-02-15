/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/test/java/org/apache/http/async/MockHttpDispatcher.java $
 * $Revision: 489367 $
 * $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
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

import java.util.Collection;

import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.protocol.HttpContext;

public class MockHttpDispatcher extends AbstractHttpDispatcher {

    protected int count_postprocess;
    protected int count_close;
    protected int count_abort;
    protected int count_send;
    protected int count_getcontext;
    protected int count_abortall;

    protected AbstractHttpHandle mock_handle; // to be returned
    protected AbstractHttpHandle last_handle;
    protected HttpResponse       last_response;


    public MockHttpDispatcher(Collection linked) {
        super(linked);
        reset();
    }

    public void reset() {
        count_postprocess = 0;
        count_close = 0;
        count_abort = 0;
        count_send = 0;
        count_getcontext = 0;
        count_abortall = 0;

        mock_handle = null;
        last_handle = null;
        last_response = null;
    }


    protected void postprocessResponse(AbstractHttpHandle handle,
                                       HttpResponse response) {
        count_postprocess++;
        last_handle = handle;
        last_response = response;
    }


    protected void closeHandle(AbstractHttpHandle handle, boolean abort) {
        if (abort)
            count_abort++;
        else
            count_close++;
        last_handle = handle;
    }


    public HttpHandle sendRequest(HttpRequest req,
                                  HttpHost target,
                                  HttpContext ctxt) {
        count_send++;
        return mock_handle;
    }


    public HttpContext getDefaultContext() {
        count_getcontext++;
        return null;
    }


    public void abortAll() {
        count_abortall++;
        last_handle = null;
        mock_handle = null;
    }


} // class MockHttpDispatcher
