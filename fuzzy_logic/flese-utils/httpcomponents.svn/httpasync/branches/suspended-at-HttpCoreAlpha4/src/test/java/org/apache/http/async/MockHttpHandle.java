/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/test/java/org/apache/http/async/MockHttpHandle.java $
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

import java.io.IOException;

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpException;
import org.apache.http.protocol.HttpContext;


public class MockHttpHandle extends AbstractHttpHandle {

    protected HttpResponse mock_response;

    protected int count_response;
    protected int count_await;

    public MockHttpHandle(AbstractHttpDispatcher dispatcher,
                          HttpRequest request,
                          HttpContext context) {
        super(dispatcher, request, context);
        reset();
    }


    public void reset() {
        mock_response = null;
        count_response = 0;
        count_await = 0;
    }


    public HttpResponse getResponse() {
        count_response++;
        return mock_response;
    }

    public HttpResponse awaitResponse() {
        count_await++;
        return mock_response;
    }

    public void close() throws HttpException, IOException {
        dispatcherCloseHandle(false);
    }

    public void abort() {
        try {
            dispatcherCloseHandle(true);
        } catch (Exception x) {
            throw new RuntimeException(x);
        }
    }

} // class MockHttpHandle
