/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpState.java,v 1.33 2004/05/13 04:03:25 mbecke Exp $
 * $Revision: 1.33 $
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

package org.apache.commons.httpclient;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.Iterator;
import org.apache.commons.httpclient.cookie.CookieSpec;
import org.apache.commons.httpclient.cookie.CookiePolicy;
import org.apache.commons.httpclient.auth.HttpAuthRealm; 
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * <p>
 * A container for HTTP attributes that may persist from request
 * to request, such as {@link Cookie cookies} and authentication
 * {@link Credentials credentials}.
 * </p>
 * <p>
 * Preemptive authentication can be turned on by using the property value of
 * #PREEMPTIVE_PROPERTY.  If left unspecified, it has the default value of
 * #PREEMPTIVE_DEFAULT.  This configurable behaviour conforms to rcf2617:
 * </p>
 * 
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Rodney Waldhoff
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author Sean C. Sullivan
 * @author <a href="mailto:becke@u.washington.edu">Michael Becke</a>
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:adrian@intencha.com">Adrian Sutton</a>
 * 
 * @version $Revision: 1.33 $ $Date: 2004-05-13 06:03:25 +0200 (Thu, 13 May 2004) $
 * 
 */
public class HttpState {

    // ----------------------------------------------------- Instance Variables

    /**
     * Map of {@link Credentials credentials} by realm that this 
     * HTTP state contains.
     */
    private HashMap credMap = new HashMap();

    /**
     * Map of {@link Credentials proxy credentials} by realm that this
     * HTTP state contains
     */
    private HashMap proxyCred = new HashMap();

    /**
     * Array of {@link Cookie cookies} that this HTTP state contains.
     */
    private ArrayList cookies = new ArrayList();

    private boolean preemptive = false;

    private int cookiePolicy = 0;
        // -------------------------------------------------------- Class Variables

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(HttpState.class);

    /**
     * Default constructor.
     */
    public HttpState() {
        super();
    }

    // ------------------------------------------------------------- Properties

    /**
     * Adds an {@link Cookie HTTP cookie}, replacing any existing equivalent cookies.
     * If the given cookie has already expired it will not be added, but existing 
     * values will still be removed.
     * 
     * @param cookie the {@link Cookie cookie} to be added
     * 
     * @see #addCookies(Cookie[])
     * 
     */
    public synchronized void addCookie(Cookie cookie) {
        LOG.trace("enter HttpState.addCookie(Cookie)");

        if (cookie != null) {
            // first remove any old cookie that is equivalent
            for (Iterator it = cookies.iterator(); it.hasNext();) {
                Cookie tmp = (Cookie) it.next();
                if (cookie.equals(tmp)) {
                    it.remove();
                    break;
                }
            }
            if (!cookie.isExpired()) {
                cookies.add(cookie);
            }
        }
    }

    /**
     * Adds an array of {@link Cookie HTTP cookies}. Cookies are added individually and 
     * in the given array order. If any of the given cookies has already expired it will 
     * not be added, but existing values will still be removed.
     * 
     * @param cookies the {@link Cookie cookies} to be added
     * 
     * @see #addCookie(Cookie)
     * 
     * 
     */
    public synchronized void addCookies(Cookie[] cookies) {
        LOG.trace("enter HttpState.addCookies(Cookie[])");

        if (cookies != null) {
            for (int i = 0; i < cookies.length; i++) {
                this.addCookie(cookies[i]);
            }
        }
    }

    /**
     * Returns an array of {@link Cookie cookies} that this HTTP
     * state currently contains.
     * 
     * @return an array of {@link Cookie cookies}.
     * 
     * @see #getCookies(String, int, String, boolean)
     * 
     */
    public synchronized Cookie[] getCookies() {
        LOG.trace("enter HttpState.getCookies()");
        return (Cookie[]) (cookies.toArray(new Cookie[cookies.size()]));
    }

    /**
     * Returns an array of {@link Cookie cookies} in this HTTP 
     * state that match the given request parameters.
     * 
     * @param domain the request domain
     * @param port the request port
     * @param path the request path
     * @param secure <code>true</code> when using HTTPS
     * 
     * @return an array of {@link Cookie cookies}.
     * 
     * @see #getCookies()
     * 
     * @deprecated use CookieSpec#match(String, int, String, boolean, Cookie)
     */
    public synchronized Cookie[] getCookies(
        String domain, 
        int port, 
        String path, 
        boolean secure
    ) {
        LOG.trace("enter HttpState.getCookies(String, int, String, boolean)");

        CookieSpec matcher = CookiePolicy.getDefaultSpec();
        ArrayList list = new ArrayList(cookies.size());
        for (int i = 0, m = cookies.size(); i < m; i++) {
            Cookie cookie = (Cookie) (cookies.get(i));
            if (matcher.match(domain, port, path, secure, cookie)) {
                list.add(cookie);
            }
        }
        return (Cookie[]) (list.toArray(new Cookie[list.size()]));
    }

    /**
     * Removes all of {@link Cookie cookies} in this HTTP state
     * that have expired according to the current system time.
     * 
     * @see #purgeExpiredCookies(java.util.Date)
     * 
     */
    public synchronized boolean purgeExpiredCookies() {
        LOG.trace("enter HttpState.purgeExpiredCookies()");
        return purgeExpiredCookies(new Date());
    }

    /**
     * Removes all of {@link Cookie cookies} in this HTTP state
     * that have expired by the specified {@link java.util.Date date}. 
     * 
     * @param date The {@link java.util.Date date} to compare against.
     * 
     * @return true if any cookies were purged.
     * 
     * @see Cookie#isExpired(java.util.Date)
     * 
     * @see #purgeExpiredCookies()
     */
    public synchronized boolean purgeExpiredCookies(Date date) {
        LOG.trace("enter HttpState.purgeExpiredCookies(Date)");
        boolean removed = false;
        Iterator it = cookies.iterator();
        while (it.hasNext()) {
            if (((Cookie) (it.next())).isExpired(date)) {
                it.remove();
                removed = true;
            }
        }
        return removed;
    }


    /**
     * Returns the current {@link CookiePolicy cookie policy} for this
     * HTTP state.
     * 
     * @return The {@link CookiePolicy cookie policy}.
     * 
     * @deprecated Use 
     *  {@link org.apache.commons.httpclient.params.HttpMethodParams#getCookiePolicy()},
     *  {@link HttpMethod#getParams()}.     
     */
    
    public int getCookiePolicy() {
        return CookiePolicy.getDefaultPolicy();
    }
    

    /**
     * Defines whether preemptive authentication should be 
     * attempted.
     * 
     * @param value <tt>true</tt> if preemptive authentication should be 
     * attempted, <tt>false</tt> otherwise. 
     * 
     * @deprecated Use 
     * {@link org.apache.commons.httpclient.params.HttpClientParams#setAuthenticationPreemptive(boolean)}, 
     * {@link HttpClient#getParams()}.
     */
    
    public void setAuthenticationPreemptive(boolean value) {
        this.preemptive = value;
    }


    /**
     * Returns <tt>true</tt> if preemptive authentication should be 
     * attempted, <tt>false</tt> otherwise.
     * 
     * @return boolean flag.
     * 
     * @deprecated Use 
     * {@link org.apache.commons.httpclient.params.HttpClientParams#isAuthenticationPreemptive()}, 
     * {@link HttpClient#getParams()}.
     */
    
    public boolean isAuthenticationPreemptive() {
        return this.preemptive;
    }
    

    /**
     * Sets the current {@link CookiePolicy cookie policy} for this HTTP
     * state to one of the following supported policies: 
     * {@link CookiePolicy#COMPATIBILITY}, 
     * {@link CookiePolicy#NETSCAPE_DRAFT} or
     * {@link CookiePolicy#RFC2109}.
     * 
     * @param policy new {@link CookiePolicy cookie policy}
     * 
     * @deprecated 
     *  Use {@link org.apache.commons.httpclient.params.HttpMethodParams#setCookiePolicy(String)},
     *  {@link HttpMethod#getParams()}.     
     */
    
    public void setCookiePolicy(int policy) {
        this.cookiePolicy = policy;
    }

    /** 
     * Sets the {@link Credentials credentials} for the given authentication 
     * realm on the given host. The <code>null</code> realm signifies default 
     * credentials for the given host, which should be used when no 
     * {@link Credentials credentials} have been explictly supplied for the 
     * challenging realm. The <code>null</code> host signifies default 
     * credentials, which should be used when no {@link Credentials credentials} 
     * have been explictly supplied for the challenging host. Any previous 
     * credentials for the given realm on the given host will be overwritten.
     * 
     * @param realm the authentication realm
     * @param host the host the realm belongs to
     * @param credentials the authentication {@link Credentials credentials} 
     * for the given realm.
     * 
     * @see #getCredentials(String, String)
     * @see #setProxyCredentials(String, String, Credentials) 
     */
    
    public synchronized void setCredentials(String realm, String host, Credentials credentials) {
        LOG.trace("enter HttpState.setCredentials(String, String, Credentials)");
        credMap.put(new HttpAuthRealm(host, realm), credentials);
    }

    /** 
     * Sets the {@link Credentials credentials} for the given authentication 
     * realm. Any previous credentials for the given realm will be overwritten.
     * 
     * @param realm the {@link HttpAuthRealm authentication realm}
     * @param credentials the authentication {@link Credentials credentials} 
     * for the given realm.
     * 
     * @see #getCredentials(HttpAuthRealm)
     * @see #setProxyCredentials(HttpAuthRealm, Credentials) 
     * 
     * @since 3.0
     */
    public synchronized void setCredentials(final HttpAuthRealm realm, Credentials credentials) {
        if (realm == null) {
            throw new IllegalArgumentException("Authentication realm token may not be null");
        }
        LOG.trace("enter HttpState.setCredentials(HttpAuthRealm, Credentials)");
        credMap.put(realm, credentials);
    }

    /**
     * Find matching {@link Credentials credentials} for the given authentication realm.
     *
     * @param map the credentials hash map
     * @param token the {@link HttpAuthRealm authentication realm token}
     * @return the credentials 
     * 
     */
    private static Credentials matchCredentials(HashMap map, HttpAuthRealm token) {
        HttpAuthRealm key = token;
        Credentials creds = (Credentials) map.get(key);
        if (creds == null && token.getScheme() != null) {
            key = new HttpAuthRealm(token.getHost(), token.getPort(), token.getRealm());
            creds = (Credentials) map.get(key);
        }
        if (creds == null && token.getRealm() != null) {
            key = new HttpAuthRealm(token.getHost(), token.getPort());
            creds = (Credentials) map.get(key);
        }
        if (creds == null && token.getPort() >= 0) {
            key = new HttpAuthRealm(token.getHost(), -1);
            creds = (Credentials) map.get(key);
        }
        if (creds == null && token.getHost() != null) {
            key = new HttpAuthRealm();
            creds = (Credentials) map.get(key);
        }
        return creds;
    }
    
    /**
     * Get the {@link Credentials credentials} for the given authentication realm on the 
     * given host.
     *
     * If the <i>realm</i> exists on <i>host</i>, return the coresponding credentials.
     * If the <i>host</i> exists with a <tt>null</tt> <i>realm</i>, return the corresponding
     * credentials.
     * If the <i>realm</i> exists with a <tt>null</tt> <i>host</i>, return the
     * corresponding credentials.  If the <i>realm</i> does not exist, return
     * the default Credentials.  If there are no default credentials, return
     * <code>null</code>.
     *
     * @param realm the authentication realm
     * @param host the host the realm is on
     * @return the credentials 
     * 
     * @see #setCredentials(String, String, Credentials)
     */
    
    public synchronized Credentials getCredentials(String realm, String host) {
        LOG.trace("enter HttpState.getCredentials(String, String");
        return matchCredentials(this.credMap, new HttpAuthRealm(host, realm));
    }

    /**
     * Get the {@link Credentials credentials} for the given authentication realm.
     *
     * @param realm the {@link HttpAuthRealm authentication realm}
     * @return the credentials 
     * 
     * @see #setCredentials(HttpAuthRealm, Credentials)
     * 
     * @since 3.0
     */
    public synchronized Credentials getCredentials(HttpAuthRealm realm) {
        if (realm == null) {
            throw new IllegalArgumentException("Authentication realm token may not be null");
        }
        LOG.trace("enter HttpState.getCredentials(HttpAuthRealm)");
        return matchCredentials(this.credMap, realm);
    }

    /**
     * Sets the {@link Credentials credentials} for the given proxy authentication 
     * realm on the given proxy host. The <code>null</code> proxy realm signifies 
     * default credentials for the given proxy host, which should be used when no 
     * {@link Credentials credentials} have been explictly supplied for the 
     * challenging proxy realm. The <code>null</code> proxy host signifies default 
     * credentials, which should be used when no {@link Credentials credentials} 
     * have been explictly supplied for the challenging proxy host. Any previous 
     * credentials for the given proxy realm on the given proxy host will be 
     * overwritten.
     *
     * @param realm the authentication realm
     * @param proxyHost the proxy host
     * @param credentials the authentication credentials for the given realm
     * 
     * @see #getProxyCredentials(String,String)
     * @see #setCredentials(String, String, Credentials)
     */
    public synchronized void setProxyCredentials(
        String realm, 
        String proxyHost, 
        Credentials credentials
    ) {
        LOG.trace("enter HttpState.setProxyCredentials(String, String, Credentials");
        proxyCred.put(new HttpAuthRealm(proxyHost, realm), credentials);
    }

    /** 
     * Sets the {@link Credentials credentials} for the given proxy authentication 
     * realm. Any previous credentials for the given realm will be overwritten.
     * 
     * @param realm the {@link HttpAuthRealm authentication realm}
     * @param credentials the authentication {@link Credentials credentials} 
     * for the given realm.
     * 
     * @see #getProxyCredentials(HttpAuthRealm)
     * @see #setCredentials(HttpAuthRealm, Credentials) 
     * 
     * @since 3.0
     */
    public synchronized void setProxyCredentials(final HttpAuthRealm realm, 
        Credentials credentials)
    {
        if (realm == null) {
            throw new IllegalArgumentException("Authentication realm token may not be null");
        }
        LOG.trace("enter HttpState.setProxyCredentials(HttpAuthRealm, Credentials)");
        proxyCred.put(realm, credentials);
    }

    /**
     * Get the {@link Credentials credentials} for the proxy host with the given 
     * authentication realm.
     *
     * If the <i>realm</i> exists on <i>host</i>, return the coresponding credentials.
     * If the <i>host</i> exists with a <tt>null</tt> <i>realm</i>, return the corresponding
     * credentials.
     * If the <i>realm</i> exists with a <tt>null</tt> <i>host</i>, return the
     * corresponding credentials.  If the <i>realm</i> does not exist, return
     * the default Credentials.  If there are no default credentials, return
     * <code>null</code>.
     * 
     * @param realm the authentication realm
     * @param proxyHost the proxy host the realm is on
     * @return the credentials 
     * @see #setProxyCredentials(String, String, Credentials)
     */
    public synchronized Credentials getProxyCredentials(String realm, String proxyHost) {
       LOG.trace("enter HttpState.getCredentials(String, String");
        return matchCredentials(this.proxyCred, new HttpAuthRealm(proxyHost, realm));
    }
    
    /**
     * Get the {@link Credentials credentials} for the given proxy authentication realm.
     *
     * @param realm the {@link HttpAuthRealm authentication realm}
     * @return the credentials 
     * 
     * @see #setProxyCredentials(HttpAuthRealm, Credentials)
     * 
     * @since 3.0
     */
    public synchronized Credentials getProxyCredentials(HttpAuthRealm realm) {
        if (realm == null) {
            throw new IllegalArgumentException("Authentication realm token may not be null");
        }
        LOG.trace("enter HttpState.getProxyCredentials(HttpAuthRealm)");
        return matchCredentials(this.proxyCred, realm);
    }

    /**
     * Returns a string representation of this HTTP state.
     * 
     * @return The string representation of the HTTP state.
     * 
     * @see java.lang.Object#toString()
     */
    public synchronized String toString() {
        StringBuffer sbResult = new StringBuffer();

        sbResult.append("[");
        sbResult.append(getCredentialsStringRepresentation(proxyCred));
        sbResult.append(" | ");
        sbResult.append(getCredentialsStringRepresentation(credMap));
        sbResult.append(" | ");
        sbResult.append(getCookiesStringRepresentation(cookies));
        sbResult.append("]");

        String strResult = sbResult.toString();

        return strResult;
    }
    
    /**
     * Returns a string representation of the credentials.
     * @param credMap The credentials.
     * @return The string representation.
     */
    private static String getCredentialsStringRepresentation(final Map credMap) {
        StringBuffer sbResult = new StringBuffer();
        Iterator iter = credMap.keySet().iterator();
        while (iter.hasNext()) {
            Object key = iter.next();
            Credentials cred = (Credentials) credMap.get(key);
            if (sbResult.length() > 0) {
                sbResult.append(", ");
            }
            sbResult.append(key);
            sbResult.append("#");
            sbResult.append(cred.toString());
        }
        return sbResult.toString();
    }
    
    /**
     * Returns a string representation of the cookies.
     * @param cookies The cookies
     * @return The string representation.
     */
    private static String getCookiesStringRepresentation(final List cookies) {
        StringBuffer sbResult = new StringBuffer();
        Iterator iter = cookies.iterator();
        while (iter.hasNext()) {
            Cookie ck = (Cookie) iter.next();
            if (sbResult.length() > 0) {
                sbResult.append("#");
            }
            sbResult.append(ck.toExternalForm());
        }
        return sbResult.toString();
    }
}
