/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/auth/HttpAuthRealm.java,v 1.8 2004/05/13 04:02:00 mbecke Exp $
 * $Revision: 1.8 $
 * $Date: 2004-05-13 06:03:25 +0200 (Thu, 13 May 2004) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2004 The Apache Software Foundation
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
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

package org.apache.commons.httpclient.auth;

/** The key used to look up authentication credentials.
 * 
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * @author <a href="mailto:adrian@intencha.com">Adrian Sutton</a>
 */
public class HttpAuthRealm {
    
    /** The authentication scheme the credentials apply to. */
    private String scheme = null;
    
    /** The realm the credentials apply to. */
    private String realm = null;
    
    /** The host the credentials apply to. */
    private String host = null;
        
    /** The port the credentials apply to. */
    private int port = -1;
        
    /** Creates a new authentication realm token for the given 
     * <tt>host</tt>, <tt>port</tt>, <tt>realm</tt>, and 
     * <tt>authentication scheme</tt>.
     * 
     * @param host the host the credentials apply to. May be set
     *   to <tt>null</tt> if credenticals are applicable to
     *   any host. 
     * @param port the port the credentials apply to. May be set
     *   to negative value if credenticals are applicable to
     *   any port. 
     * @param realm the realm the credentials apply to. May be set 
     *   to <tt>null</tt> if credenticals are applicable to
     *   any realm. 
     * @param scheme the authentication scheme the credentials apply to. 
     *   May be set to <tt>null</tt> if credenticals are applicable to
     *   any authentication scheme. 
     * 
     * @since 3.0
     */
    public HttpAuthRealm(final String host, int port, 
        final String realm, final String scheme)
    {
        this.host = host;
        this.port = port;
        this.realm = realm;
        this.scheme = scheme;
    }
    
    /** Creates a new authentication realm token  for the given 
     * <tt>host</tt>, <tt>port</tt>, <tt>realm</tt>, and any
     * authentication scheme.
     * 
     * @param host the host the credentials apply to. May be set
     *   to <tt>null</tt> if credenticals are applicable to
     *   any host. 
     * @param port the port the credentials apply to. May be set
     *   to negative value if credenticals are applicable to
     *   any port. 
     * @param realm the realm the credentials apply to. May be set 
     *   to <tt>null</tt> if credenticals are applicable to
     *   any realm. 
     * 
     * @since 3.0
     */
    public HttpAuthRealm(final String host, int port, final String realm) {
        this(host, port, realm, null);
    }
    
    /** Creates a new authentication realm token for the given 
     * <tt>host</tt>, <tt>port</tt>, any realm name, and any
     * authentication scheme.
     * 
     * @param host the host the credentials apply to. May be set
     *   to <tt>null</tt> if credenticals are applicable to
     *   any host. 
     * @param port the port the credentials apply to. May be set
     *   to negative value if credenticals are applicable to
     *   any port. 
     * 
     * @since 3.0
     */
    public HttpAuthRealm(final String host, int port) {
        this(host, port, null, null);
    }
    
    /** Creates a new authentication realm token  for the given 
     * <tt>host</tt>, <tt>realm</tt>, any port, and any authentication
     * scheme. 
     * 
     * @param host the host the credentials apply to. May be set
     *   to <tt>null</tt> if credenticals are applicable to
     *   any host. 
     * @param realm the realm the credentials apply to. May be set 
     *   to <tt>null</tt> if credenticals are applicable to
     *   any realm. 
     */
    public HttpAuthRealm(final String host, final String realm) {
        this(host, -1, realm, null);
    }
    
    /** 
     * Creates a new authentication realm token that matches any 
     * authentication realm.
     * 
     * @since 3.0
     */
    public HttpAuthRealm() {
        this(null, -1, null, null);
    }
    
    /** 
     * Creates a copy of the given authentication realm token.
     * 
     * @since 3.0
     */
    public HttpAuthRealm(final HttpAuthRealm token) {
        this(token.host, token.port, token.realm, token.scheme);
    }
    
    /**
     * @return the host
     * 
     * @since 3.0
     */
    public String getHost() {
        return this.host;
    }

    /**
     * @return the port
     * 
     * @since 3.0
     */
    public int getPort() {
        return this.port;
    }

    /**
     * @return the realm name
     * 
     * @since 3.0
     */
    public String getRealm() {
        return this.realm;
    }

    /**
     * @return the scheme type
     * 
     * @since 3.0
     */
    public String getScheme() {
        return this.scheme;
    }

    /** Determines if the given parameters match.  Note that <tt>null</tt> acts as a
     * wildcard so if either of the parameters are <tt>null</tt>, it is considered a match.
     * 
     * @param p1 the parameter
     * @param p2 the other parameter
     * @return boolean true if the parameters match, otherwise false.
     */
    private static boolean paramsMatchIgnoreCase(final String p1, final String p2) {
        return p1 == null || p2 == null || p1.equalsIgnoreCase(p2);
    }

    /** Determines if the given parameters match.  Note that <tt>null</tt> acts as a
     * wildcard so if either of the parameters are <tt>null</tt>, it is considered a match.
     * 
     * @param p1 the parameter
     * @param p2 the other parameter
     * @return boolean true if the parameters match, otherwise false.
     */
    private static boolean paramsMatch(final String p1, final String p2) {
        return p1 == null || p2 == null || p1.equals(p2);
    }

    /** Determines if the given parameters match.  Note that negative value acts as a
     * wildcard so if either of the parameters are negative, it is considered a match.
     * 
     * @param p1 the parameter
     * @param p2 the other parameter
     * @return boolean true if the parameters match, otherwise false.
     */
    private static boolean paramsMatch(int p1, int p2) {
        return p1 < 0 || p2 < 0 || p1 == p2;
    }

    /**
     * @see java.lang.Object#equals(Object)
     */
    public boolean equals(Object o) {
        if (o == null) {
            return false;
        }
        if (o == this) {
            return true;
        }
        if (!(o instanceof HttpAuthRealm)) {
            return super.equals(o);
        }
        HttpAuthRealm that = (HttpAuthRealm) o;
        return 
        paramsMatchIgnoreCase(this.host, that.host) 
          && paramsMatch(this.port, that.port)
          && paramsMatch(this.realm, that.realm)
          && paramsMatchIgnoreCase(this.scheme, that.scheme);
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        if (this.scheme != null) {
            buffer.append(this.scheme);
            buffer.append(' ');
        }
        if (this.realm != null) {
            buffer.append("authentication realm '");
            buffer.append(this.realm);
            buffer.append("'");
        } else {
            buffer.append("default authentication realm ");
        }
        if (this.host != null) {
            buffer.append('@');
            buffer.append(this.host);
            if (this.port >= 0) {
                buffer.append(':');
                buffer.append(this.port);
            }
        }
        return buffer.toString();
    }
    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return ((this.host != null) ? this.host.toLowerCase().hashCode() : 0) + 
               ((this.port >= 0) ? this.port : -1) +
               ((this.realm != null) ? this.realm.hashCode() : 0) +
               ((this.scheme != null) ? this.scheme.toLowerCase().hashCode() : 0);
    }
}
