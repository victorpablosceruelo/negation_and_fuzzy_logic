/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/test/java/org/apache/http/async/MockAsyncHttpProcessor.java $
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

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpProcessor;

public class MockAsyncHttpProcessor extends AsyncHttpProcessor {

    protected int count_prepare;
    protected int count_send;
    protected int count_receive;
    protected int count_finish;


    public MockAsyncHttpProcessor(HttpProcessor processor) {
        super(processor);
        reset();
    }


    public void reset() {
         count_prepare = 0;
         count_send = 0;
         count_receive = 0;
         count_finish = 0;
    }



    protected void doPrepareRequest(HttpRequest request,
                                    HttpContext context) {
        count_prepare++;
    }

    protected void asyncSendRequest(HttpRequest               request,
                                    HttpAsyncClientConnection connection,
                                    HttpContext               context) {
        count_send++;
    }

    protected
        HttpResponse doReceiveResponse(HttpRequest               request,
                                       HttpAsyncClientConnection connection,
                                       HttpContext               context) {
        count_receive++;
        return null;
    }

    protected void doFinishResponse(HttpResponse response,
                                    HttpContext context) {
        count_finish++;
    }

} // class MockAsyncHttpProcessor
