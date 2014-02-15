/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/HttpAsyncClientConnection.java $
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

import org.apache.http.HttpClientConnection;
import org.apache.http.HttpHost;
import org.apache.http.params.HttpParams;

/**
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $ $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 * 
 * @since 4.0
 */
public interface HttpAsyncClientConnection extends HttpClientConnection {

    void open(HttpParams params) throws IOException;

    HttpHost getTargetHost();

    void setTargetHost(HttpHost targethost);
    
}
