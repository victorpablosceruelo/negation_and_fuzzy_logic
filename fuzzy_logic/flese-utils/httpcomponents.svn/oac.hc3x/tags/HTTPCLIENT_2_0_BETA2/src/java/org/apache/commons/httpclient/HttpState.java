/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpState.java,v 1.22 2003/05/08 18:39:07 olegk Exp $
 * $Revision: 1.22 $
 * $Date: 2003-05-08 20:39:07 +0200 (Thu, 08 May 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
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
 * to request, such as {@link Cookie}s and authentication
 * {@link Credentials}.
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
 * @version $Revision: 1.22 $ $Date: 2003-05-08 20:39:07 +0200 (Thu, 08 May 2003) $
 * 
 */
public class HttpState {

    // ----------------------------------------------------- Instance Variables

    /**
     * Whether I should attempt to authenticate preemptively.
     */
    private boolean preemptive;

    /**
     * The boolean property name to turn on preemptive authentication.
     */
    public static final String PREEMPTIVE_PROPERTY = 
        "httpclient.authentication.preemptive";

    /**
     * The default property value for #PREEMPTIVE_PROPERTY.
     */
    public static final String PREEMPTIVE_DEFAULT = "false";

    /**
     * My {@link Credentials Credentials}s, by realm.
     */
    private HashMap credMap = new HashMap();

    /**
     * My proxy {@link Credentials Credentials}, by realm.
     */
    private HashMap proxyCred = new HashMap();

    /**
     * The default authentication realm.
     */
    public static final HttpAuthRealm DEFAULT_AUTH_REALM = new HttpAuthRealm(null, null); 

    /**
     * My {@link Cookie Cookie}s.
     */
    private ArrayList cookies = new ArrayList();
    /**
     * My cookie policy.  Default is {@link CookiePolicy.RFC2109}
     */
    private int cookiePolicy = CookiePolicy.RFC2109;

    /** The current connection manager */
    private HttpConnectionManager httpConnectionManager;

    // -------------------------------------------------------- Class Variables

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(HttpState.class);

    /**
     * Constructor for HttpState.
     */
    public HttpState() {
        
        super();
        
        this.cookiePolicy = CookiePolicy.getDefaultPolicy();

        // check the preemptive policy
        // TODO: this needs to be a service from some configuration class
        String preemptiveDefault =
            System.getProperties().getProperty(PREEMPTIVE_PROPERTY,
                    PREEMPTIVE_DEFAULT);
        preemptiveDefault = preemptiveDefault.trim().toLowerCase();

        if (!(preemptiveDefault.equals("true")
                    || preemptiveDefault.equals("false"))) { // property problem
            LOG.warn("Configuration property " + PREEMPTIVE_PROPERTY
                     + " must be either true or false.  Using default: "
                     + PREEMPTIVE_DEFAULT);
            preemptiveDefault = PREEMPTIVE_DEFAULT;
        }
        this.preemptive = ("true".equals(preemptiveDefault));
    }

    // ------------------------------------------------------------- Properties

    /**
     * Add a cookie.
     * If the given <i>cookie</i> has already expired,
     * deletes the corresponding existing cookie (if any).
     * 
     * @param cookie the {@link Cookie} to add
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
     * Add zero or more cookies
     * If any given <i>cookie</i> has already expired,
     * deletes the corresponding existing cookie (if any).
     * 
     * @param newcookies the {@link Cookie}s to add
     * 
     * @see #addCookie(Cookie)
     * 
     * 
     */
    public synchronized void addCookies(Cookie[] newcookies) {
        LOG.trace("enter HttpState.addCookies(Cookie[])");

        if (newcookies != null) {
            for (int i = 0; i < newcookies.length; i++) {
                this.addCookie(newcookies[i]);
            }
        }
    }

    /**
     * Obtain an array of my {@link Cookie}s.
     * 
     * @return an array of my {@link Cookie}s.
     * 
     * @see #getCookies(String, int, String, boolean, java.util.Date)
     * 
     */
    public synchronized Cookie[] getCookies() {
        LOG.trace("enter HttpState.getCookies()");
        return (Cookie[]) (cookies.toArray(new Cookie[cookies.size()]));
    }

    /**
     * Obtain an array of my {@link Cookie}s that
     * match the given request parameters.
     * 
     * @param domain the request domain
     * @param port the request port
     * @param path the request path
     * @param secure <code>true</code> when using HTTPS
     * @param now the {@link Date} by which expiration is determined
     * @return an array of my {@link Cookie}s.
     * 
     * @see Cookie#matches
     * @see #getCookies()
     * 
     * @deprecated use HttpState.getCookies(String, int, String, boolean)
     */
    public synchronized Cookie[] getCookies(
        String domain, 
        int port, 
        String path, 
        boolean secure, 
        Date now
    ) {
        return getCookies(domain, port, path, secure);
    }


    /**
     * Obtain an array of my {@link Cookie}s that
     * match the given request parameters.
     * 
     * @param domain the request domain
     * @param port the request port
     * @param path the request path
     * @param secure <code>true</code> when using HTTPS
     * @return an array of my {@link Cookie}s.
     * 
     * @see Cookie#matches
     * @see #getCookies()
     * 
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
     * Remove all of my {@link Cookie}s that
     * have expired according to the current
     * system time.
     * 
     * @see #purgeExpiredCookies(java.util.Date)
     * 
     */
    public synchronized boolean purgeExpiredCookies() {
        LOG.trace("enter HttpState.purgeExpiredCookies()");
        return purgeExpiredCookies(new Date());
    }

    /**
     * Remove all of my {@link Cookie}s that have expired by the specified
     * <i>date</i>.
     * 
     * @param date The date to compare against.
     * @return true if any cookies were purged.
     * @see Cookie#isExpired(java.util.Date)
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
     * Return the current {@link CookiePolicy}
     * @return The cookie policy.
     */
    
    public int getCookiePolicy() {
        return this.cookiePolicy;
    }
    

    /**
     * Defines whether preemptive authentication should be 
     * attempted or not.
     * 
     * @param value boolean flag
     */
    
    public void setAuthenticationPreemptive(boolean value) {
        this.preemptive = value;
    }


    /**
     * Return <tt>true</tt> if preemptive authentication should be 
     * attempted, otherwise return <tt>false</tt>
     * 
     * @return boolean flag.
     */
    
    public boolean isAuthenticationPreemptive() {
        return this.preemptive;
    }
    

    /**
     * Set the {@link CookiePolicy} to one of {@link
     * CookiePolicy#COMPATIBILITY}, {@link CookiePolicy#NETSCAPE_DRAFT} or
     * {@link CookiePolicy#RFC2109}
     * @param policy new cookie policy
     */
    
    public void setCookiePolicy(int policy) {
        this.cookiePolicy = policy;
    }

    /**
     * Set the Credentials for the given authentication realm.
     *
     * When <i>realm</i> is <code>null</code>, I'll use the given
     * <i>credentials</i> when no other {@link Credentials} have
     * been supplied for the given challenging realm.
     * (I.e., use a <code>null</code> realm to set the "default"
     * credentials.)
     * <p>
     * Any previous credentials for this realm will be overwritten.
     *
     * @deprecated This method does not distinguish between realms with the
     * same name on different hosts.  Use
     * {@link HttpState#setCredentials(String, Credentials)} instead.
     * 
     * @param realm the authentication realm
     * @param credentials the authentication credentials for the given realm
     * 
     * @see #getCredentials(String, String)
     * @see #setProxyCredentials(String, String, Credentials)
     * 
     */
    
    public synchronized void setCredentials(String realm, Credentials credentials) {
        LOG.trace("enter HttpState.setCredentials(String, Credentials)");
        setCredentials(realm, null, credentials);
    }
    
    /** Sets the credentials for <tt>realm</tt> on <tt>host</tt>.
     * with no host.
     * 
     * When <i>realm</i> is <code>null</code>, I'll use the given
     * <i>credentials</i> when no other {@link Credentials} have
     * been supplied for the given challenging realm.
     * (I.e., use a <code>null</code> realm to set the "default"
     * credentials.)
     * <p>
     * Any previous credentials for this realm will be overwritten.
     * 
     * @param realm the authentication realm
     * @param host the host the realm belongs to
     * @param credentials the authentication credentials for the given realm.
     * 
     * @see #getCredentials(String, String)
     * @see #setProxyCredentials(String, String, Credentials) 
     */
    
    public synchronized void setCredentials(String realm, String host, Credentials credentials) {
        LOG.trace(
            "enter HttpState.setCredentials(String realm, String host, Credentials credentials)");
        credMap.put(new HttpAuthRealm(host, realm), credentials);
    }


    /**
     * Find matching credentials for the given authentication realm and host.
     *
     * If the <i>realm</i> exists on <i>host</i>, return the coresponding credentials.
     * If the <i>host</i> exists with a <tt>null</tt> <i>realm</i>, return the corresponding
     * credentials.
     * If the <i>realm</i> exists with a <tt>null</tt> <i>host</i>, return the
     * corresponding credentials.  If the <i>realm</i> does not exist, return
     * the default Credentials.  If there are no default credentials, return
     * <code>null</code>.
     *
     * @param map the credentials hash map
     * @param realm the authentication realm
     * @param host the host the realm is on
     * @return the credentials 
     * 
     */
    private static Credentials matchCredentials(HashMap map, String realm, String host) {
        HttpAuthRealm entry = new HttpAuthRealm(host, realm);
        Credentials creds = (Credentials) map.get(entry);
        if (creds == null && host != null && realm != null) {
            entry = new HttpAuthRealm(host, null);
            creds = (Credentials) map.get(entry);
            if (creds == null) {
                entry = new HttpAuthRealm(null, realm);
                creds = (Credentials) map.get(entry);
            }
        }
        if (creds == null) {
            creds = (Credentials) map.get(DEFAULT_AUTH_REALM);
        }
        return creds;
    }
    
    /**
     * Get the Credentials for the given authentication realm.
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
     * 
     */
    
    public synchronized Credentials getCredentials(String realm, String host) {
        LOG.trace("enter HttpState.getCredentials(String, String");
        return matchCredentials(this.credMap, realm, host);
    }

    /**
     * Get the Credentials for the given authentication realm.
     *
     * If the <i>realm</i> exists on <i>host</i>, return the coresponding credentials.
     * If the <i>realm</i> exists with a <tt>null</tt> <i>host</i>, return the
     * corresponding credentials.  If the <i>realm</i> does not exist, return
     * the default Credentials.  If there is no default credentials, return
     * <code>null</code>.
     *
     * @deprecated This method does not distinguish between realms on different
     * servers with the same name.  Use {@link #getCredentials(String, String)}
     * instead.
     * 
     * @param realm the authentication realm
     * @return the credentials 
     * 
     * @see #setCredentials(String, String, Credentials)
     * 
     */
    
    public synchronized Credentials getCredentials(String realm) {
        LOG.trace("enter HttpState.getCredentials(String)");

        return getCredentials(realm, null);
    }

    /**
     * Set the for the proxy with the given authentication realm.
     *
     * When <i>realm</i> is <code>null</code>, I'll use the given
     * <i>credentials</i> when no other {@link Credentials} have
     * been supplied for the given challenging realm.
     * (I.e., use a <code>null</code> realm to set the "default"
     * credentials.) Realms rarely make much sense with proxies, so
     * <code>null</code> is normally a good choice here.
     * <p>
     * Any previous credentials for this realm will be overwritten.
     *
     * @deprecated This method does not differentiate between realms with
     * the same name on different servers.  Use
     * {@link #setProxyCredentials(String, String, Credentials)} instead.
     * 
     * @param realm the authentication realm
     * @param credentials the authentication credentials for the given realm
     * 
     * @see #getProxyCredentials(String)
     * @see #setCredentials(String, Credentials)
     * 
     */
    
    public synchronized void setProxyCredentials(String realm, Credentials credentials) {
        LOG.trace("enter HttpState.setProxyCredentials(String, credentials)");
        setProxyCredentials(realm, null, credentials);
    }
    
    /**
     * Set the credentials for the proxy with the given authentication realm.
     *
     * When <i>realm</i> and <i>proxyHost</i> are <code>null</code>, I'll use the given
     * <i>credentials</i> when no other {@link Credentials} have
     * been supplied for the given challenging realm.
     * (I.e., use a <code>null</code> realm to set the "default"
     * credentials.) Realms rarely make much sense with proxies, so
     * <code>null</code> is normally a good choice here.
     * <p>
     * Any previous credentials for this realm will be overwritten.
     *
     * @param realm the authentication realm
     * @param proxyHost the proxy host
     * @param credentials the authentication credentials for the given realm
     * 
     * @see #getProxyCredentials(String)
     * @see #setCredentials(String, Credentials)
     * 
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
     * Get the Credentials for the proxy with the given authentication realm.
     *
     * If the <i>realm</i> exists, return the coresponding credentials.  If the 
     * <i>realm</i> does not exist, return the default Credentials.  If there is 
     * no default credentials, return <code>null</code>.
     * 
     * @deprecated This method does not distinguish between realms on different hosts.
     * Use {@link #getProxyCredentials(String, String)} instead.
     *
     * @param realm the authentication realm
     * @return the credentials 
     * @see #setProxyCredentials(String, String, Credentials)
     */
    
    public synchronized Credentials getProxyCredentials(String realm) {
        LOG.trace("enter HttpState.getProxyCredentials(String)");
        return getProxyCredentials(realm, null);
    }
    
    /**
     * Get the Credentials for the proxy with the given authentication realm on the given
     * <i>host</i>.
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
        return matchCredentials(this.proxyCred, realm, proxyHost);
    }
    
    /**
     * Return a string representation of this object.
     * @return The string representation.
     * @see java.lang.Object#toString()
     */
    public synchronized String toString() {
        StringBuffer sbResult = new StringBuffer();

        sbResult.append("[");
        sbResult.append(getProxyCredentialsStringRepresentation(proxyCred));
        sbResult.append(" | ");
        sbResult.append(getCredentialsStringRepresentation(proxyCred));
        sbResult.append(" | ");
        sbResult.append(getCookiesStringRepresentation(cookies));
        sbResult.append("]");

        String strResult = sbResult.toString();

        return strResult;
    }
    
    /**
     * Return a string representation of the proxy credentials
     * @param proxyCredMap The proxy credentials
     * @return StringBuffer The string representation.
     */
    private static StringBuffer getProxyCredentialsStringRepresentation(final Map proxyCredMap) {
        StringBuffer sbResult = new StringBuffer();
        Iterator iter = proxyCredMap.keySet().iterator();
        while (iter.hasNext()) {
            Object key = iter.next();
            Credentials cred = (Credentials) proxyCredMap.get(key);
            if (sbResult.length() > 0) {
                sbResult.append(", ");
            }
            sbResult.append(key);
            sbResult.append("#");
            sbResult.append(cred.toString());
        }
        return sbResult;
    }
    
    /**
     *  Return a string representation of the credentials.
     * @param credMap The credentials.
     * @return StringBuffer The string representation.
     */
    private static StringBuffer getCredentialsStringRepresentation(final Map credMap) {
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
        return sbResult;
    }
    
    /**
     * Return a string representation of the cookies.
     * @param cookies The cookies
     * @return StringBuffer The string representation.
     */
    private static StringBuffer getCookiesStringRepresentation(final List cookies) {
        StringBuffer sbResult = new StringBuffer();
        Iterator iter = cookies.iterator();
        while (iter.hasNext()) {
            Cookie ck = (Cookie) iter.next();
            if (sbResult.length() > 0) {
                sbResult.append("#");
            }
            sbResult.append(ck.toExternalForm());
        }
        return sbResult;
    }
    
    /**
     * Returns the httpConnectionManager.
     * @return HttpConnectionManager
     * 
     * @deprecated Connection manager is controlled by the HttpClient class.
     * Use {@link HttpClient#getHttpConnectionManager()} instead.
     * 
     * @since 2.0
     */
    public synchronized HttpConnectionManager getHttpConnectionManager() {
        return httpConnectionManager;
    }

    /**
     * Sets the httpConnectionManager.
     * @param httpConnectionManager The httpConnectionManager to set
     * 
     * @deprecated Connection manager is controlled by the HttpClient class.
     * Use {@link HttpClient#setHttpConnectionManager(HttpConnectionManager)} instead.
     *
     * @since 2.0
     */
    public synchronized void setHttpConnectionManager(
        HttpConnectionManager httpConnectionManager
    ) {
        this.httpConnectionManager = httpConnectionManager;
    }
}
