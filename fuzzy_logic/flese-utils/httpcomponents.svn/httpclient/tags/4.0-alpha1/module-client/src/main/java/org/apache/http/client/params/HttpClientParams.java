/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/main/java/org/apache/http/client/params/HttpClientParams.java $
 * $Revision: 555245 $
 * $Date: 2007-07-11 13:14:49 +0200 (Wed, 11 Jul 2007) $
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
 * @version $Revision: 555245 $
 * 
 * @since 4.0
 */
public class HttpClientParams {

    /**
     * Defines the timeout in milliseconds used when retrieving an instance of 
     * {@link org.apache.http.conn.ManagedClientConnection} from the
     * {@link org.apache.http.conn.ClientConnectionManager}.
     * <p>
     * This parameter expects a value of type {@link Long}.
     * </p>
     */ 
    public static final String CONNECTION_MANAGER_TIMEOUT = "http.connection-manager.timeout"; 

    /**
     * Defines the class name of the default {@link org.apache.http.conn.ClientConnectionManager}
     * <p>
     * This parameter expects a value of type {@link String}.
     * </p>
     */ 
    public static final String CONNECTION_MANAGER_FACTORY = "http.connection-manager.factory"; 

    /** 
     * Defines whether redirects should be handled automatically
     * <p>
     * This parameter expects a value of type {@link Boolean}.
     * </p>
     */
    public static final String HANDLE_REDIRECTS = "http.protocol.handle-redirects";

    /**
     * Defines whether relative redirects should be rejected.
     * <p>
     * This parameter expects a value of type {@link Boolean}.
     * </p>
     */
    public static final String REJECT_RELATIVE_REDIRECT = "http.protocol.reject-relative-redirect"; 

    /** 
     * Defines the maximum number of redirects to be followed. 
     * The limit on number of redirects is intended to prevent infinite loops. 
     * <p>
     * This parameter expects a value of type {@link Integer}.
     * </p>
     */
    public static final String MAX_REDIRECTS = "http.protocol.max-redirects";

    /** 
     * Defines whether circular redirects (redirects to the same location) should be allowed. 
     * The HTTP spec is not sufficiently clear whether circular redirects are permitted, 
     * therefore optionally they can be enabled
     * <p>
     * This parameter expects a value of type {@link Boolean}.
     * </p>
     */
    public static final String ALLOW_CIRCULAR_REDIRECTS = "http.protocol.allow-circular-redirects";

    /**
     * Defines whether authentication should be handled automatically.
     * <p>
     * This parameter expects a value of type {@link Boolean}.
     * </p>
     */
    public static final String HANDLE_AUTHENTICATION = "http.protocol.handle-authentication";

    /**
     * Defines whether authentication should be attempted preemptively.
     * <p>
     * This parameter expects a value of type {@link Boolean}.
     * </p>
     */
    public static final String PREEMPTIVE_AUTHENTICATION = "http.protocol.authentication-preemptive";

    /**
     * The key used to look up the list of IDs of supported
     * {@link AuthPolicy authentication schemes} in their order of preference.
     * The scheme IDs are stored in a {@link java.util.Collection} as
     * instances of {@link java.lang.String}.
     * 
     * <p>
     * If several schemes are returned in the <tt>WWW-Authenticate</tt> 
     * or <tt>Proxy-Authenticate</tt> header, this parameter defines which
     * {@link AuthPolicy authentication schemes} takes precedence over others.
     * The first item in the collection represents the most preferred 
     * {@link AuthPolicy authentication scheme}, the last item represents
     * the ID of the least preferred one.
     * </p>
     */
    public static final String AUTH_SCHEME_PRIORITY = "http.protocol-auth-scheme-priority";

    /**
     * Defines the name of the cookie specification to be used for HTTP state management.
     * <p>
     * This parameter expects a value of type {@link String}.
     * </p>
     */
    public static final String COOKIE_POLICY = "http.protocol.cookie-policy";
    
    /**
     * Defines the virtual host name.
     * <p>
     * This parameter expects a value of type {@link org.apache.http.HttpHost}. 
     * </p>
     */
    public static final String VIRTUAL_HOST = "http.virtual-host"; 

    /**
     * Defines the request headers to be sent per default with each request.
     * <p>
     * This parameter expects a value of type {@link java.util.Collection}. The 
     * collection is expected to contain {@link org.apache.http.Header}s. 
     * </p>
     */
    public static final String DEFAULT_HEADERS = "http.default-headers"; 
    
    /**
     * Defines the default host. The default value will be used if the target host is
     * not explicitly specified in the request URI.
     * <p>
     * This parameter expects a value of type {@link org.apache.http.HttpHost}.
     * </p>
     */
    public static final String DEFAULT_HOST = "http.default-host";
    
    /**
     * Defines the default proxy. The default value will be used if the proxy
     * information is not explicitly specified in the request route. 
     * <p>
     * This parameter expects a value of type {@link org.apache.http.HttpHost}.
     * </p>
     */
    public static final String DEFAULT_PROXY = "http.default-proxy";

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
        return params.getLongParameter(CONNECTION_MANAGER_TIMEOUT, 0);
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
        params.setLongParameter(CONNECTION_MANAGER_TIMEOUT, timeout);
    }

    public static boolean isRedirecting(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getBooleanParameter(HANDLE_REDIRECTS, true); 
    }

    public static void setRedirecting(final HttpParams params, boolean value) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setBooleanParameter(HANDLE_REDIRECTS, value); 
    }
    
    public static boolean isAuthenticating(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getBooleanParameter(HANDLE_AUTHENTICATION, true); 
    }

    public static void setAuthenticating(final HttpParams params, boolean value) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setBooleanParameter(HANDLE_AUTHENTICATION, value); 
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
        return params.getBooleanParameter(PREEMPTIVE_AUTHENTICATION, false); 
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
        params.setBooleanParameter(PREEMPTIVE_AUTHENTICATION, value); 
    }
    
    public static String getCookiePolicy(final HttpParams params) { 
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        String cookiePolicy = (String) params.getParameter(COOKIE_POLICY);
        if (cookiePolicy == null) {
            return CookiePolicy.BROWSER_COMPATIBILITY;
        }
        return cookiePolicy;
    }
    
    public static void setCookiePolicy(final HttpParams params, final String cookiePolicy) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setParameter(COOKIE_POLICY, cookiePolicy);
    }

}
