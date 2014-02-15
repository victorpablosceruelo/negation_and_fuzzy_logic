/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpState.java,v 1.15 2003/01/23 22:47:47 jsdever Exp $
 * $Revision: 1.15 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
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
 * 4. The names "The Jakarta Project", "HttpClient", and "Apache Software
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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * <p>
 * A container for HTTP attributes that may persist from request
 * to request, such as {@link Cookie}s and authentication
 * {@link Credentials}.
 * </p>
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Rodney Waldhoff
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author Sean C. Sullivan
 * @author <a href="mailto:becke@u.washington.edu">Michael Becke</a>
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 1.15 $ $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
 * 
 */
public class HttpState {

    // ----------------------------------------------------- Instance Variables

    /**
     * My {@link Credentials Credentials}s, by realm.
     */
    private HashMap credMap = new HashMap();

    /**
     * My proxy {@link Credentials Credentials}, by realm.
     */
    private HashMap proxyCred = new HashMap();

    /**
     * My {@link Cookie Cookie}s.
     */
    private ArrayList cookies = new ArrayList();
    /**
     * My cookie policy.
     */
    private int cookiePolicy = CookiePolicy.RFC2109;

    private HttpConnectionManager httpConnectionManager;

    // -------------------------------------------------------- Class Variables

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(HttpState.class);

    /**
     * Constructor for HttpState.
     */
    public HttpState() {
        
        super();
        
        this.httpConnectionManager = new SimpleHttpConnectionManager();
        this.cookiePolicy = CookiePolicy.getDefaultPolicy();
        
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
        log.trace("enter HttpState.addCookie(Cookie)");

        if (cookie != null) {
            // first remove any old cookie that is equivalent
            for (Iterator it = cookies.iterator();it.hasNext(); ) {
                Cookie tmp = (Cookie) it.next();
                if(cookie.equals(tmp)) {
                    it.remove();
                    break;
                }
            }
            if(!cookie.isExpired()) {
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
        log.trace("enter HttpState.addCookies(Cookie[])");

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
        log.trace("enter HttpState.getCookies()");
        return (Cookie[])(cookies.toArray(new Cookie[cookies.size()]));
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
        log.trace("enter HttpState.getCookies(String, int, String, boolean)");

		CookieSpec matcher = CookiePolicy.getDefaultSpec();
        ArrayList list = new ArrayList(cookies.size());
        for(int i=0,m=cookies.size();i<m;i++) {
            Cookie cookie = (Cookie)(cookies.get(i));
            if(matcher.match(domain, port, path, secure, cookie)) {
                list.add(cookie);
            }
        }
        return (Cookie[])(list.toArray(new Cookie[list.size()]));
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
        log.trace("enter HttpState.purgeExpiredCookies()");
        return purgeExpiredCookies(new Date());
    }

    /**
     * Remove all of my {@link Cookie}s that
     * have expired by the specified <i>date</i>.
     * 
     * @see Cookie#isExpired(java.util.Date)
     * @see #purgeExpiredCookies()
     * 
     */
    public synchronized boolean purgeExpiredCookies(Date date) {
        log.trace("enter HttpState.purgeExpiredCookies(Date)");
        boolean removed = false;
        Iterator it = cookies.iterator();
        while(it.hasNext()) {
            if( ((Cookie)(it.next())).isExpired(date) ) {
                it.remove();
                removed = true;
            }
        }
        return removed;
    }


    /**
     * @return cookie policy <tt>(COMPATIBILITY | NETSCAPE_DRAFT | RFC2109)</tt>
     */
    
    public int getCookiePolicy()
    {
        return this.cookiePolicy;
    }
    

    /**
     * @param new cookie policy <tt>(COMPATIBILITY | NETSCAPE_DRAFT | RFC2109)</tt>
     */
    
    public void setCookiePolicy(int policy)
    {
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
     * @param realm the authentication realm
     * @param credentials the authentication credentials for the given realm
     * 
     * @see #getCredentials(String)
     * @see #setProxyCredentials(String, Credentials)
     * 
     */
    public synchronized void setCredentials(String realm, Credentials credentials) {
        log.trace("enter HttpState.setCredentials(String, Credentials)");
        credMap.put(realm,credentials);
    }


    /**
     * Get the Credentials for the given authentication realm.
     *
     * If the <i>realm</i> exists, return the coresponding credentials.  If the 
     * <i>realm</i> does not exist, return the default Credentials.  If there is 
     * no default credentials, return <code>null</code>.
     *
     * @param realm the authentication realm
     * @return the credentials 
     * 
     * @see #setCredentials(String, Credentials)
     * 
     */
    public synchronized Credentials getCredentials(String realm) {
        log.trace("enter HttpState.getCredentials(String)");

        Credentials creds = (Credentials) credMap.get(realm);
        if (creds == null) {
            creds = (Credentials) credMap.get(null);
        }
        return creds;
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
     * @param realm the authentication realm
     * @param credentials the authentication credentials for the given realm
     * 
     * @see #getProxyCredentials(String)
     * @see #setCredentials(String, Credentials)
     * 
     */
    public synchronized void setProxyCredentials(String realm, Credentials credentials) {
        log.trace("enter HttpState.setProxyCredentials(String, credentials)");
        proxyCred.put(realm, credentials);
    }


    /**
     * Get the Credentials for the proxy with the given authentication realm.
     *
     * If the <i>realm</i> exists, return the coresponding credentials.  If the 
     * <i>realm</i> does not exist, return the default Credentials.  If there is 
     * no default credentials, return <code>null</code>.
     *
     * @param realm the authentication realm
     * @return the credentials 
     * @see #setProxyCredentials
     */
    public synchronized Credentials getProxyCredentials(String realm) {
        log.trace("enter HttpState.getProxyCredentials(String)");
        Credentials creds = (Credentials) proxyCred.get(realm);
        if (creds == null) {
            creds = (Credentials) proxyCred.get(null);
        }
        return creds;
    }
    
    public synchronized String toString()
    {
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
    
    private static StringBuffer getProxyCredentialsStringRepresentation(final Map proxyCredMap)
    {
    	StringBuffer sbResult = new StringBuffer();
    	Iterator iter = proxyCredMap.keySet().iterator();
    	while (iter.hasNext())
    	{
    		String key = (String) iter.next();
    		Credentials cred = (Credentials) proxyCredMap.get(key);
    		if (sbResult.length() > 0)
    		{
    			sbResult.append(", ");
    		}
    		sbResult.append(key);
    		sbResult.append("#");
    		sbResult.append(cred.toString());
    	}
    	return sbResult;
    }

    private static StringBuffer getCredentialsStringRepresentation(final Map credMap)
    {
    	StringBuffer sbResult = new StringBuffer();
    	Iterator iter = credMap.keySet().iterator();
    	while (iter.hasNext())
    	{
    		String key = (String) iter.next();
    		Credentials cred = (Credentials) credMap.get(key);
    		if (sbResult.length() > 0)
    		{
    			sbResult.append(", ");
    		}
    		sbResult.append(key);
    		sbResult.append("#");
    		sbResult.append(cred.toString());
    	}
    	return sbResult;
    }
    
    private static StringBuffer getCookiesStringRepresentation(final List cookies)
    {
    	StringBuffer sbResult = new StringBuffer();
    	Iterator iter = cookies.iterator();
    	while (iter.hasNext())
    	{
    		Cookie ck = (Cookie) iter.next();
    		if (sbResult.length() > 0)
    		{
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
     * @since 2.0
     */
    public synchronized HttpConnectionManager getHttpConnectionManager() {
        return httpConnectionManager;
    }

    /**
     * Sets the httpConnectionManager.
     * @param httpConnectionManager The httpConnectionManager to set
     * 
     * @since 2.0
     */
    public synchronized void setHttpConnectionManager(
        HttpConnectionManager httpConnectionManager
    ) {
        this.httpConnectionManager = httpConnectionManager;
    }

}
