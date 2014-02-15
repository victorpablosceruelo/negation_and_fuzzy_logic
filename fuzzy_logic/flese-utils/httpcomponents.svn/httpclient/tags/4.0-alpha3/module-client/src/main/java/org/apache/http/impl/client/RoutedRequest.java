/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha3/module-client/src/main/java/org/apache/http/impl/client/RoutedRequest.java $
 * $Revision: 613654 $
 * $Date: 2008-01-20 20:00:19 +0100 (Sun, 20 Jan 2008) $
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

package org.apache.http.impl.client;

import org.apache.http.HttpRequest;
import org.apache.http.conn.routing.HttpRoute;


/**
 * A request with the route along which it should be sent.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines to avoid svn diff problems -->
 * @version $Revision: 613654 $
 *
 * @since 4.0
 */
public interface RoutedRequest {

    /**
     * Obtains the request.
     *
     * @return the request
     */
    HttpRequest getRequest()
        ;


    /**
     * Obtains the route.
     *
     * @return the route
     */
    HttpRoute getRoute()
        ;


    /**
     * Trivial default implementation of a routed request.
     */
    public static class Impl implements RoutedRequest {

        protected final HttpRequest request;
        protected final HttpRoute route;

        /**
         * Creates a new routed request.
         *
         * @param req   the request
         * @param rou   the route
         */
        public Impl(HttpRequest req, HttpRoute rou) {
            this.request = req;
            this.route   = rou;
        }

        // non-javadoc, see interface
        public final HttpRequest getRequest() {
            return request;
        }

        // non-javadoc, see interface
        public final HttpRoute getRoute() {
            return route;
        }
    } // class Impl


} // interface RoutedRequest
