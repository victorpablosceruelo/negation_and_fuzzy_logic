/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/Authenticator.java,v 1.6 2001/08/08 20:37:28 rwaldhoff Exp $
 * $Revision: 1.6 $
 * $Date: 2001-08-08 22:37:28 +0200 (Wed, 08 Aug 2001) $
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

import java.util.StringTokenizer;
import java.util.NoSuchElementException;
import org.apache.commons.httpclient.log.*;

/**
 * Authenticate helper.
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 */
public class Authenticator {
    static private final Log log = LogSource.getInstance("org.apache.commons.httpclient.Authenticator");


    // ----------------------------------------------------- Instance Variables


    /**
     * Base 64 encoder.
     */
    protected static Base64 base64 = new Base64();


    // ------------------------------------------------------------- Properties


    /**
     * Generate a response to the given challenge.
     *
     * @param state State
     * @param credentials Credentials to use to answser the challenge
     * @return String response to the challenge
     * @deprecated Use {@link #challengeResponse(java.lang.String,org.apache.commons.httpclient.State)}
     */
    public static String challengeResponse(State state,
                                           Credentials credentials)
        throws HttpException {
        log.debug("Authenticator.challengeResponse(State,Credentials)");

        if (credentials == null)
            throw new HttpException(HttpException.NO_CREDENTIALS_GIVEN);

        String challenge = state.getAuthenticateToken();
        if (challenge == null) {
            return null;
        }

        int space = challenge.indexOf(' ');
        if (space < 0)
            return null;

        String authScheme = challenge.substring(0, space);

        if ("basic".equalsIgnoreCase(authScheme)) {
            return basic(state, credentials);
        } else if ("digest".equalsIgnoreCase(authScheme)) {
            throw new UnsupportedOperationException("Digest authentication is not supported.");
        } else {
            throw new UnsupportedOperationException("Authentication type \"" + authScheme + "\" is not recognized.");
        }
    }


    public static String challengeResponse(String challenge, State state) throws HttpException {
        log.debug("Authenticator.challengeResponse(String, State)");

        if(challenge == null) { return null; }

        int space = challenge.indexOf(' ');
        if(space < 0) {
            throw new HttpException("Unable to parse authentication challenge \"" + challenge + "\", expected space");
        }
        String authScheme = challenge.substring(0, space);

        if ("basic".equalsIgnoreCase(authScheme)) {
            // parse the realm from the authentication challenge
            // XXX FIX ME XXX
            // Note that this won't work if there is more than one
            // realm within the challenge
            // We could probably make it a bit more flexiable in
            // parsing as well.
            if(challenge.length() < space + 1) {
                throw new HttpException("Unable to parse authentication challenge \"" + challenge + "\", expected realm");
            }
            String realmstr = challenge.substring(space+1,challenge.length());
            realmstr.trim();
            log.debug("Parsing realm from \"" + realmstr + "\".");
            String realm = realmstr.substring("realm=\"".length(),realmstr.length()-1);
            log.debug("Parsed realm \"" + realm + "\" from challenge \"" + challenge + "\".");

            return basic(realm,state);
        } else if ("digest".equalsIgnoreCase(authScheme)) {
            throw new UnsupportedOperationException("Digest authentication is not supported.");
        } else {
            throw new UnsupportedOperationException("Authentication type \"" + authScheme + "\" is not recognized.");
        }
    }

    /**
     * Generate a basic response.
     *
     * @param credentials Credentials to use to answser the challenge
     * @deprecated Use {@link #basic(java.lang.String,org.apache.commons.httpclient.State)}
     */
    public static String basic(State state, Credentials credentials) {
        log.debug("Authenticator.basic(State,Credentials)");
        String authString = credentials.getUserName() + ":"
            + credentials.getPassword();
        return "Basic " + new String(base64.encode(authString.getBytes()));

    }

    public static String basic(String realm, State state) throws HttpException {
        log.debug("Authenticator.basic(String,State)");
        Credentials cred = state.getCredentials(realm);
        if(null == cred) {
            if(log.isInfoEnabled()) {
                log.info("No credentials found for realm \"" + realm + "\", attempting to use default credentials.");
            }
            cred = state.getDefaultCredentials();
            if(null == cred) {
                throw new HttpException(HttpException.NO_CREDENTIALS_GIVEN);
            }
        }
        String authString = cred.getUserName() + ":" + cred.getPassword();
        return "Basic " + new String(base64.encode(authString.getBytes()));
    }

}
