/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/test/java/org/apache/http/async/MockHttpNotificationHandler.java $
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

import org.apache.http.HttpResponse;

public class MockHttpNotificationHandler implements HttpNotificationHandler {

    protected int count_response;
    protected int count_easy_error;
    protected int count_fatal_error;

    protected HttpHandle last_handle;
    protected HttpResponse last_response;
    protected Throwable last_problem;

    protected boolean do_proceed;


    public void reset() {

        count_response = 0;
        count_easy_error = 0;
        count_fatal_error = 0;

        last_handle = null;
        last_response = null;
        last_problem = null;
    }


    public void notifyResponse(HttpHandle handle, HttpResponse nqrsp) {

        count_response++;

        last_handle = handle;
        last_response = nqrsp;
    }


    public boolean notifyProblem(HttpHandle handle, Throwable problem,
                                 boolean nonfatal) {

        if (nonfatal)
            count_easy_error++;
        else
            count_fatal_error++;

        last_handle = handle;
        last_problem = problem;

        return do_proceed;
    }

} // class MockHttpNotificationHandler
