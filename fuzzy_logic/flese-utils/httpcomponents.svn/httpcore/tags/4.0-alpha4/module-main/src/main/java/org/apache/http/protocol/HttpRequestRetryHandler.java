/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-main/src/main/java/org/apache/http/protocol/HttpRequestRetryHandler.java $
 * $Revision: 496070 $
 * $Date: 2007-01-14 13:18:34 +0100 (Sun, 14 Jan 2007) $
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

package org.apache.http.protocol;

import java.io.IOException;


/**
 * A handler for determining if an HttpRequest should be retried after a 
 * recoverable exception during execution.
 * 
 * <p>
 * Classes implementing this interface must synchronize access to shared
 * data as methods of this interfrace may be executed from multiple threads 
 * </p>
 * 
 * @author Michael Becke
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public interface HttpRequestRetryHandler {

    /**
     * Determines if a method should be retried after an IOException
     * occurs during execution.
     * 
     * @param exception the exception that occurred
     * @param executionCount the number of times this method has been 
     * unsuccessfully executed
     * @param context the context for the request execution
     * 
     * @return <code>true</code> if the method should be retried, <code>false</code>
     * otherwise
     */
    boolean retryRequest(IOException exception, int executionCount, HttpContext context);

}
