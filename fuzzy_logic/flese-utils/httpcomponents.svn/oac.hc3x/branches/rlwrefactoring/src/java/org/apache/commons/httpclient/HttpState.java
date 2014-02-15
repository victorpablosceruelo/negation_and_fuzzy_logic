/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpState.java,v 1.1.2.3 2001/10/01 21:39:36 rwaldhoff Exp $
 * $Revision: 1.1.2.3 $
 * $Date: 2001-10-01 23:39:36 +0200 (Mon, 01 Oct 2001) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Tomcat", and "Apache Software
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

import java.io.IOException;
import java.util.Vector;
import java.util.Enumeration;
import java.util.Date;

import java.util.HashMap;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Iterator;

/**
 * <p>
 * A container for HTTP attributes that may persist from request
 * to request, such as {@link Cookie}s and authentication
 * {@link Credentials}.
 * </p>
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Rodney Waldhoff
 * @version $Revision: 1.1.2.3 $ $Date: 2001-10-01 23:39:36 +0200 (Mon, 01 Oct 2001) $
 */
public class HttpState {

    // ----------------------------------------------------- Instance Variables

    protected HashMap credMap = new HashMap();

     /**
      * Cookies.
      */
    protected ArrayList cookies = new ArrayList();

    // ------------------------------------------------------------- Properties

    /**
     * Add a cookie.
     * If the cookie has already expired, deletes the
     * corresponding existing cookie, if any.
     */
    public void addCookie(Cookie cookie) {
        if (cookie != null) {
            // let's remove the cookie if it's already saved
            for (Iterator it = cookies.iterator();it.hasNext(); ) {
                Cookie tmp = (Cookie) it.next();
                if (cookie.getDomain().equals(tmp.getDomain()) &&
                    cookie.getName().equals(tmp.getName())) {
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
     * Add a number of cookies
     * If a cookie has already expired, deletes any
     * corresponding existing cookie.
     */
    public void addCookies(Cookie[] newcookies) {
        if (newcookies != null) {
            for (int i = 0; i < newcookies.length; i++) {
                this.addCookie(newcookies[i]);
            }
        }
    }

    public Cookie[] getCookies() {
        return (Cookie[])(cookies.toArray(new Cookie[cookies.size()]));
    }

    public Cookie[] getCookies(String domain, int port, String path, boolean secure, Date now) {
        ArrayList list = new ArrayList(cookies.size());
        for(int i=0,m=cookies.size();i<m;i++) {
            Cookie c = (Cookie)(cookies.get(i));
            if(c.matches(domain,port,path,secure,now)) {
                list.add(c);
            }
        }
        return (Cookie[])(list.toArray(new Cookie[list.size()]));
    }

    public boolean purgeExpiredCookies() {
        return purgeExpiredCookies(new Date());
    }

    public boolean purgeExpiredCookies(Date now) {
        boolean removed = false;
        Iterator it = cookies.iterator();
        while(it.hasNext()) {
            if( ((Cookie)(it.next())).isExpired(now) ) {
                it.remove();
                removed = true;
            }
        }
        return removed;
    }

    /**
     * Set the {@link Credentials} for the given authentication realm.
     */
    public void setCredentials(String realm, Credentials credentials) {
        credMap.put(realm,credentials);
    }


    /**
     * Get the {@link Credentials} for the given authentication realm.
     */
    public Credentials getCredentials(String realm) {
        return (Credentials)(credMap.get(realm));
    }

    /**
     * Set the default {@link Credentials}, used when no
     * other realm is specified, or when no credential is
     * found to match the given realm.
     *
     * @deprecated Use
     * {@link #setCredentials(java.lang.String,org.apache.commons.httpclient.Credentials)}
     * instead.
     */
    public void setDefaultCredentials(Credentials credentials) {
        this.setCredentials(null,credentials);
    }


    /**
     * Get the defualt {@link Credentials}.
     */
    public Credentials getDefaultCredentials() {
        return this.getCredentials(null);
    }

}
