/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/oac.hc3x/trunk/src/test/org/apache/commons/httpclient/server/HttpService.java $
 * $Revision: 608014 $
 * $Date: 2008-01-02 06:48:53 +0100 (Wed, 02 Jan 2008) $
 *
 * ====================================================================
 *
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient.server;

import java.io.IOException;

/**
 * Defines an HTTP request/response service for the SimpleHttpServer
 * 
 * @author Oleg Kalnichevski
 */
public interface HttpService {
    /**
     * This interface represents a serice to process HTTP requests.
     * 
     * @param request       The HTTP request object.
     * @param response      The HTTP response object.
     * @return true if this service was able to handle the request, false otherwise.
     * 
     * @throws IOException
     */
    public boolean process(
        final SimpleRequest request, final SimpleResponse response) 
            throws IOException;
}
