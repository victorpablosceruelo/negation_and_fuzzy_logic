/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha2/module-client/src/main/java/org/apache/http/impl/client/BasicCookieStore.java $
 * $Revision: 558148 $
 * $Date: 2007-07-20 23:19:39 +0200 (Fri, 20 Jul 2007) $
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

package org.apache.http.impl.client;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.Iterator;

import org.apache.http.client.CookieStore;
import org.apache.http.cookie.Cookie;
import org.apache.http.cookie.CookieIdentityComparator;

/**
 * Default implementation of {@link CookieStore}
 * 
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Rodney Waldhoff
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author Sean C. Sullivan
 * @author <a href="mailto:becke@u.washington.edu">Michael Becke</a>
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:adrian@intencha.com">Adrian Sutton</a>
 * 
 * @since 4.0
 */
public class BasicCookieStore implements CookieStore {

    private final ArrayList cookies;

    private final Comparator cookieComparator;
    
    // -------------------------------------------------------- Class Variables

    /**
     * Default constructor.
     */
    public BasicCookieStore() {
        super();
        this.cookies = new ArrayList();
        this.cookieComparator = new CookieIdentityComparator();
    }

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
        if (cookie != null) {
            // first remove any old cookie that is equivalent
            for (Iterator it = cookies.iterator(); it.hasNext();) {
                Cookie tmp = (Cookie) it.next();
                if (cookieComparator.compare(cookie, tmp) == 0) {
                    it.remove();
                    break;
                }
            }
            if (!cookie.isExpired(new Date())) {
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
     */
    public synchronized void addCookies(Cookie[] cookies) {
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
     */
    public synchronized Cookie[] getCookies() {
        return (Cookie[]) (cookies.toArray(new Cookie[cookies.size()]));
    }

    /**
     * Removes all of {@link Cookie cookies} in this HTTP state
     * that have expired by the specified {@link java.util.Date date}. 
     * 
     * @return true if any cookies were purged.
     * 
     * @see Cookie#isExpired(Date)
     */
    public synchronized boolean clearExpired(final Date date) {
        if (date == null) {
            return false;
        }
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

    public String toString() {
        return cookies.toString();
    }
    
    /**
     * Clears all cookies.
     */
    public synchronized void clear() {
        cookies.clear();
    }
    
}
