/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/auth/AuthState.java,v 1.2 2004/04/18 23:51:36 jsdever Exp $
 * $Revision: 1.2 $
 * $Date: 2004-04-19 01:51:38 +0200 (Mon, 19 Apr 2004) $
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

/**
 * This class provides detailed information about the state of the
 * authentication process.
 * 
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * @since 3.0
 */
public class AuthState {

    /** Actual authentication scheme */
    private AuthScheme authScheme = null;

    /** Whether preemtive authentication is attempted */
    private boolean preemptive  = false; 
      
    /**
     * Default constructor.
     * 
     */
    public AuthState() {
        super();
    }

    /**
     * Preemptively assigns Basic authentication scheme.
     */
    public void setPreemptive() {
        if (this.authScheme != null) {
            throw new IllegalStateException("Authentication state already initialized");
        }
        this.authScheme = AuthPolicy.getAuthScheme("basic");
        this.preemptive = true;
    }

    /**
     * Invalidates the authentication state by resetting its parameters.
     */
    public void invalidate() {
        this.authScheme = null;
        this.preemptive = false;
    }
    
    /**
     * Tests if preemptive authentication is used.
     * 
     * @return <tt>true</tt> if using the default Basic {@link AuthScheme 
     * authentication scheme}, <tt>false</tt> otherwise.
     */
    public boolean isPreemptive() {
        return this.preemptive;
    }
    
    /**
     * Assigns the given {@link AuthScheme authentication scheme}.
     * 
     * @param authScheme the {@link AuthScheme authentication scheme}
     */
    public void setAuthScheme(final AuthScheme authScheme) {
        this.authScheme = authScheme;
        this.preemptive = false;
    }

    /**
     * Returns the {@link AuthScheme authentication scheme}.
     * 
     * @return {@link AuthScheme authentication scheme}
     */
    public AuthScheme getAuthScheme() {
        return authScheme;
    }
    
    /**
     * Returns the authentication realm.
     * 
     * @return the name of the authentication realm
     */
    public String getRealm() {
        if (this.authScheme != null) {
            return this.authScheme.getRealm();
        } else {
            return null;
        }
    }
}
