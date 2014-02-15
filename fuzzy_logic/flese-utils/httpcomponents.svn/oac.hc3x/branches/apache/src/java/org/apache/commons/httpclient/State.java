/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/State.java,v 1.1 2001/04/25 18:42:52 remm Exp $
 * $Revision: 1.1 $
 * $Date: 2001-04-25 20:42:48 +0200 (Wed, 25 Apr 2001) $
 *
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
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

/**
 * Session state.
 * 
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 */
public class State {
    
    
    // ----------------------------------------------------- Instance Variables
    
    
    /**
     * Lock tokens.
     */
    protected Hashtable lockTokens = new Hashtable();
    
    
    /**
     * Authenticate token.
     */
    protected String authenticateToken = null;
    
    
     /**
      * Cookies.
      */
    protected Vector cookies = new Vector();
    
    
    /**
     * URL encoding switch.
     */
    protected boolean encodeURLs = true;


    /**
     * URL encoding charset.
     */
    protected String URLEncodingCharset = "UTF8";
    
    
    /**
     * URL decoding charset.
     */
    protected String URLDecodingCharset = "UTF8";


    // ------------------------------------------------------------- Properties
    
    
    /**
     * Add a cookie
     */
    public void addCookie(Cookie cookie) {
        
        if (cookie != null) {
            
            boolean found = false;
            
            // let's remove the cookie if it's already saved
            for (Enumeration e = cookies.elements(); 
                 !found && e.hasMoreElements(); ) {
                Cookie tmp = (Cookie) e.nextElement();
                if (cookie.getDomain().equals(tmp.getDomain()) && 
                    cookie.getName().equals(tmp.getName())) {
                    found = true;
                    cookies.removeElement(tmp);
                }
            }
            cookies.addElement(cookie);
        }
        
    }
    
    
    /**
     * Add a number of cookies
     */
    public void addCookies(Cookie[] cookies) {
        if (cookies != null) {
            for (int i = 0; i < cookies.length; i++) {
                this.addCookie(cookies[i]);
            }
        }
    }
    
    
    // FIXME: this breaks encapsulation on the cookie vector
    public Vector getCookies() {
        return cookies;
    }
    
    
    /**
     * Add a lock token.
     * 
     * @param uri Uri
     * @param value Lock token value
     */
    public void addLock(String uri, String value) {
        
        Vector currentLocks = (Vector) lockTokens.get(uri);
        if (currentLocks == null)
            currentLocks = new Vector();
        currentLocks.addElement(value);
        lockTokens.put(uri, currentLocks);
        
    }
    
    
    /**
     * Remove a lock.
     * 
     * @param uri Uri
     * @param value LockToken value
     */
    public void removeLock(String uri, String value) {
        
        Vector currentLocks = (Vector) lockTokens.get(uri);
        if (currentLocks == null)
            return;
        currentLocks.removeElement(value);
        
    }
    
    
    /**
     * Remove locks.
     * 
     * @param uri Uri
     */
    public void removeLocks(String uri) {
        
        lockTokens.remove(uri);
        
    }
    
    
    /**
     * Get locks
     * 
     * @param uri Uri
     * @return Enumeration of lock tokens
     */
    public Enumeration getLocks(String uri) {
        
        Vector result = (Vector) lockTokens.get(uri);
        if (result == null)
            result = new Vector();
        return result.elements();
        
    }
    
    
    /**
     * Authenticate token setter.
     * 
     * @param authenticateToken Authenticate token
     */
    public void setAuthenticateToken(String authenticateToken) {
        this.authenticateToken = authenticateToken;
    }
    
    
    /**
     * Authenticate token accessor.
     * 
     * @return String authenticate token
     */
    public String getAuthenticateToken() {
        return authenticateToken;
    }
    
    
    /**
     * Set the URL encoding flag.
     */
    public void setEncodeURLs(boolean encodeURLs) {
        this.encodeURLs = encodeURLs;
    }


    /**
     * Set URL encoding charset.
     */
    public void setURLEncodingCharset(String URLEncodingCharset) {
        this.URLEncodingCharset = URLEncodingCharset;
    }


    /**
     * Set URL decoding charset.
     */
    public void setURLDecodingCharset(String URLDecodingCharset) {
        this.URLDecodingCharset = URLDecodingCharset;
    }


    // --------------------------------------------------------- Public Methods
    
    
    /**
     * URL encode.
     */
    public String URLEncode(String url) {
        if (encodeURLs) {
            return URLUtil.URLEncode(url, URLEncodingCharset);
        } else {
            return url;
        }
    }


    /**
     * URL decode.
     */
    public String URLDecode(String url) {
        return URLUtil.URLDecode(url, URLDecodingCharset);
    }


}
