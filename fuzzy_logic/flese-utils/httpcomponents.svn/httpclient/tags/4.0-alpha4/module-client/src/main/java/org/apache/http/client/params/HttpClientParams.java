/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha4/module-client/src/main/java/org/apache/http/client/params/HttpClientParams.java $
 * $Revision: 613865 $
 * $Date: 2008-01-21 13:04:30 +0100 (Mon, 21 Jan 2008) $
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

package org.apache.http.client.params;

import org.apache.http.params.HttpParams;

/**
 * An adaptor for accessing HTTP client parameters in {@link HttpParams}.
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 613865 $
 * 
 * @since 4.0
 */
public class HttpClientParams {

    private HttpClientParams() {
        super();
    }

    /**
     * Returns the timeout in milliseconds used when retrieving a
     * {@link org.apache.http.conn.ManagedClientConnection} from the
     * {@link org.apache.http.conn.ClientConnectionManager}.
     * 
     * @return timeout in milliseconds.
     */ 
    public static long getConnectionManagerTimeout(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getLongParameter
            (ClientPNames.CONNECTION_MANAGER_TIMEOUT, 0);
    }

    /**
     * Sets the timeout in milliseconds used when retrieving a
     * {@link org.apache.http.conn.ManagedClientConnection} from the
     * {@link org.apache.http.conn.ClientConnectionManager}.
     * 
     * @param timeout the timeout in milliseconds
     */ 
    public static void setConnectionManagerTimeout(final HttpParams params, long timeout) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setLongParameter
            (ClientPNames.CONNECTION_MANAGER_TIMEOUT, timeout);
    }

    public static boolean isRedirecting(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getBooleanParameter
            (ClientPNames.HANDLE_REDIRECTS, true); 
    }

    public static void setRedirecting(final HttpParams params, boolean value) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setBooleanParameter
            (ClientPNames.HANDLE_REDIRECTS, value); 
    }
    
    public static boolean isAuthenticating(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getBooleanParameter
            (ClientPNames.HANDLE_AUTHENTICATION, true); 
    }

    public static void setAuthenticating(final HttpParams params, boolean value) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setBooleanParameter
            (ClientPNames.HANDLE_AUTHENTICATION, value); 
    }
    
    /**
     * Returns <tt>true</tt> if authentication should be attempted preemptively, 
     * <tt>false</tt> otherwise.
     * 
     * @return <tt>true</tt> if authentication should be attempted preemptively,
     *   <tt>false</tt> otherwise.
     */
    public static boolean isAuthenticationPreemptive(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getBooleanParameter
            (ClientPNames.PREEMPTIVE_AUTHENTICATION, false); 
    }

    /**
     * Sets whether authentication should be attempted preemptively.
     * 
     * @param value <tt>true</tt> if authentication should be attempted preemptively,
     *   <tt>false</tt> otherwise.
     */
    public static void setAuthenticationPreemptive(final HttpParams params, boolean value) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setBooleanParameter
            (ClientPNames.PREEMPTIVE_AUTHENTICATION, value); 
    }
    
    public static String getCookiePolicy(final HttpParams params) { 
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        String cookiePolicy = (String)
            params.getParameter(ClientPNames.COOKIE_POLICY);
        if (cookiePolicy == null) {
            return CookiePolicy.BEST_MATCH;
        }
        return cookiePolicy;
    }
    
    public static void setCookiePolicy(final HttpParams params, final String cookiePolicy) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setParameter(ClientPNames.COOKIE_POLICY, cookiePolicy);
    }

}
