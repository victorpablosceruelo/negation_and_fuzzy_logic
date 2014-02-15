/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha2/module-client/src/main/java/org/apache/http/conn/params/HttpConnectionManagerParams.java $
 * $Revision: 576068 $
 * $Date: 2007-09-16 12:25:01 +0200 (Sun, 16 Sep 2007) $
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
 */

package org.apache.http.conn.params;

import java.util.HashMap;
import java.util.Map;

import org.apache.http.conn.HttpRoute;
import org.apache.http.params.HttpParams;

/**
 * This class represents a collection of HTTP protocol parameters applicable
 * to client-side
 * {@link org.apache.http.conn.ClientConnectionManager connection managers}. 
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * @author Michael Becke
 * 
 * @version $Revision: 576068 $
 * 
 * @since 4.0
 *
 * @see ConnManagerPNames
 */
public final class HttpConnectionManagerParams {

    /** The default maximum number of connections allowed per host */
    public static final int DEFAULT_MAX_HOST_CONNECTIONS = 2;   // Per RFC 2616 sec 8.1.4

    /** The default maximum number of connections allowed overall */
    public static final int DEFAULT_MAX_TOTAL_CONNECTIONS = 20;

    /** A key to represent the default for route-specific settings. */
    private static final String ROUTE_DEFAULT = "*Route*Default*";



    /**
     * Sets the default maximum number of connections allowed for routes.
     *
     * @param max The default maximum.
     * 
     * @see ConnManagerPNames#MAX_HOST_CONNECTIONS
     */
    public static void setDefaultMaxConnectionsPerHost(final HttpParams params,
                                                       final int max) {
        setMaxPerHost(params, ROUTE_DEFAULT, max);
    }

    /**
     * Sets the maximum number of connections to be used for a given route.
     * 
     * @param route     the route to set the maximum for
     * @param max       the maximum number of connections,
     *                  must be greater than 0
     * 
     * @see ConnManagerPNames#MAX_HOST_CONNECTIONS
     */
    public static void setMaxConnectionsPerHost(final HttpParams params,
                                                final HttpRoute route,
                                                final int max) {
        if (route == null) {
            throw new IllegalArgumentException
                ("Route must not be null.");
        }
        setMaxPerHost(params, route, max);
    }


    /**
     * Internal setter for a max-per-host value.
     *
     * @param params    the parameters in which to set a max-per-host value
     * @param key       the key, either an {@link HttpRoute} or
     *                  {@link #ROUTE_DEFAULT}
     * @param max       the value to set for that key
     */
    private static void setMaxPerHost(HttpParams params,
                                      Object key, int max) {
        if (params == null) {
            throw new IllegalArgumentException
                ("HTTP parameters must not be null.");
        }
        if (max <= 0) {
            throw new IllegalArgumentException
                ("The maximum must be greater than 0.");
        }
        
        Map currentValues = (Map) params.getParameter
            (ConnManagerPNames.MAX_HOST_CONNECTIONS);
        // param values are meant to be immutable so we'll make a copy
        // to modify
        Map newValues = null;
        if (currentValues == null) {
            newValues = new HashMap();
        } else {
            newValues = new HashMap(currentValues);
        }
        newValues.put(key, new Integer(max));
        params.setParameter(ConnManagerPNames.MAX_HOST_CONNECTIONS, newValues);
    }

    
    /**
     * Gets the default maximum number of connections allowed for a given
     * host config.
     *
     * @return The default maximum.
     * 
     * @see ConnManagerPNames#MAX_HOST_CONNECTIONS
     */
    public static int getDefaultMaxConnectionsPerHost(
        final HttpParams params) {
        return getMaxPerHost( params, ROUTE_DEFAULT);
    }

    /**
     * Gets the maximum number of connections allowed for a specific route.
     * If the value has not been specified for the given route, the default
     * value will be returned.
     * 
     * @param route     the route for which to get the maximum connections
     *
     * @return The maximum number of connections allowed for the given route.
     * 
     * @see ConnManagerPNames#MAX_HOST_CONNECTIONS
     */
    public static int getMaxConnectionsPerHost(final HttpParams params,
                                               final HttpRoute route) {
        if (route == null) {
            throw new IllegalArgumentException
                ("Route must not be null.");
        }
        return getMaxPerHost(params, route);
    }


    /**
     * Internal getter for a max-per-host value.
     *
     * @param params    the parameters from which to get a max-per-host value
     * @param key       the key, either an {@link HttpRoute} or
     *                  {@link #ROUTE_DEFAULT}
     *
     * @return  the maximum for that key, or the default maximum
     */
    private static int getMaxPerHost(final HttpParams params,
                                     final Object key) {
        
        if (params == null) {
            throw new IllegalArgumentException
                ("HTTP parameters must not be null.");
        }

        // if neither a specific nor a default maximum is configured...
        int result = DEFAULT_MAX_HOST_CONNECTIONS;

        Map m = (Map) params.getParameter
            (ConnManagerPNames.MAX_HOST_CONNECTIONS);
        if (m != null) {
            Integer max = (Integer) m.get(key);
            if ((max == null) && (key != ROUTE_DEFAULT)) {
                // no specific maximum, get the configured default
                max = (Integer) m.get(ROUTE_DEFAULT);
            }
            if (max != null) {
                result = max.intValue();
            }
        }

        return result;
    }


    /**
     * Sets the maximum number of connections allowed.
     *
     * @param maxTotalConnections The maximum number of connections allowed.
     * 
     * @see ConnManagerPNames#MAX_TOTAL_CONNECTIONS
     */
    public static void setMaxTotalConnections(
            final HttpParams params,
            int maxTotalConnections) {
        if (params == null) {
            throw new IllegalArgumentException
                ("HTTP parameters must not be null.");
        }
        params.setIntParameter(
            ConnManagerPNames.MAX_TOTAL_CONNECTIONS,
            maxTotalConnections);
    }

    /**
     * Gets the maximum number of connections allowed.
     *
     * @return The maximum number of connections allowed.
     * 
     * @see ConnManagerPNames#MAX_TOTAL_CONNECTIONS
     */
    public static int getMaxTotalConnections(
            final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException
                ("HTTP parameters must not be null.");
        }
        return params.getIntParameter(
            ConnManagerPNames.MAX_TOTAL_CONNECTIONS,
            DEFAULT_MAX_TOTAL_CONNECTIONS);
    }

}
