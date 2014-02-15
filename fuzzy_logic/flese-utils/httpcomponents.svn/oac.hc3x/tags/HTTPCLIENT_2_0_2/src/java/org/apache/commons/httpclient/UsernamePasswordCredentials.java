/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/UsernamePasswordCredentials.java,v 1.11.2.1 2004/02/22 18:21:13 olegk Exp $
 * $Revision: 1.11.2.1 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient;

/**
 * <p>Username and password {@link Credentials}.</p>
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Sean C. Sullivan
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * 
 * @version $Revision: 1.11.2.1 $ $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 * 
 */
public class UsernamePasswordCredentials implements Credentials {

    // ----------------------------------------------------------- Constructors

    /**
     * Default constructor.
     */
    public UsernamePasswordCredentials() {
    }


    /**
     * The constructor with the username and password combined string argument.
     *
     * @param usernamePassword the username:password formed string
     * @see #toString
     */
    public UsernamePasswordCredentials(String usernamePassword) {
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
     * User name property setter.
     *
     * @param userName
     * @see #getUserName()
     */
    public void setUserName(String userName) {
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
        result.append((this.userName == null) ? "null" : this.userName);
        result.append(":");
        result.append((this.password == null) ? "null" : this.password);
        return result.toString();
    }

}

