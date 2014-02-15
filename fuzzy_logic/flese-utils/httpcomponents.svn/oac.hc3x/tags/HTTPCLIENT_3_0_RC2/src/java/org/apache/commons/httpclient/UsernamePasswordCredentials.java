/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/UsernamePasswordCredentials.java,v 1.14 2004/04/18 23:51:35 jsdever Exp $
 * $Revision: 155418 $
 * $Date: 2005-02-26 14:01:52 +0100 (Sat, 26 Feb 2005) $
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

/**
 * <p>Username and password {@link Credentials}.</p>
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Sean C. Sullivan
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 155418 $ $Date: 2005-02-26 14:01:52 +0100 (Sat, 26 Feb 2005) $
 * 
 */
public class UsernamePasswordCredentials implements Credentials {

    // ----------------------------------------------------------- Constructors

    /**
     * Default constructor.
     * 
     * @deprecated Do not use. Null user name no longer allowed
     */
    public UsernamePasswordCredentials() {
        super();
    }


    /**
     * The constructor with the username and password combined string argument.
     *
     * @param usernamePassword the username:password formed string
     * @see #toString
     */
    public UsernamePasswordCredentials(String usernamePassword) {
        super();
        if (usernamePassword == null) {
            throw new IllegalArgumentException("Username:password string may not be null");            
        }
        int atColon = usernamePassword.indexOf(':');
        if (atColon >= 0) {
            this.userName = usernamePassword.substring(0, atColon);
            this.password = usernamePassword.substring(atColon + 1);
        } else {
            this.userName = usernamePassword;
        }
    }


    /**
     * The constructor with the username and password arguments.
     *
     * @param userName the user name
     * @param password the password
     */
    public UsernamePasswordCredentials(String userName, String password) {
        super();
        if (userName == null) {
            throw new IllegalArgumentException("Username may not be null");            
        }
        this.userName = userName;
        this.password = password;
    }

    // ----------------------------------------------------- Instance Variables

    /**
     * User name.
     */
    private String userName;


    /**
     * Password.
     */
    private String password;


    // ------------------------------------------------------------- Properties


    /**
     * User name property setter. User name may not be null.
     *
     * @param userName
     * @see #getUserName()
     * 
     * @deprecated Do not use. The UsernamePasswordCredentials objects should be immutable
     */
    public void setUserName(String userName) {
        if (userName == null) {
            throw new IllegalArgumentException("Username may not be null");            
        }
        this.userName = userName;
    }


    /**
     * User name property getter.
     *
     * @return the userName
     * @see #setUserName(String)
     */
    public String getUserName() {
        return userName;
    }


    /**
     * Password property setter.
     *
     * @param password
     * @see #getPassword()
     * 
     * @deprecated Do not use. The UsernamePasswordCredentials objects should be immutable
     */
    public void setPassword(String password) {
        this.password = password;
    }


    /**
     * Password property getter.
     *
     * @return the password
     * @see #setPassword(String)
     */
    public String getPassword() {
        return password;
    }

    
    /**
     * Get this object string.
     *
     * @return the username:password formed string
     */
    public String toString() {
        StringBuffer result = new StringBuffer();
        result.append(this.userName);
        result.append(":");
        result.append((this.password == null) ? "null" : this.password);
        return result.toString();
    }

}

