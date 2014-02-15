/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/auth/AuthSchemeBase.java,v 1.3 2003/04/06 22:31:53 jsdever Exp $
 * $Revision: 1.3 $
 * $Date: 2004-01-17 06:43:14 +0100 (Sat, 17 Jan 2004) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
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

package org.apache.commons.httpclient.auth;

/**
 * <p>
 * Abstract authentication scheme class that implements {@link AuthScheme}
 * interface and provides a default contstructor.
 * </p>
 *
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 */
public abstract class AuthSchemeBase implements AuthScheme {

    /**
     * Original challenge string as received from the server.
     */
    private String challenge = null;

    /**
     * Constructor for an abstract authetication schemes.
     * 
     * @param challenge authentication challenge
     * 
     * @throws MalformedChallengeException is thrown if the authentication challenge
     * is malformed
     */
    public AuthSchemeBase(final String challenge) 
      throws MalformedChallengeException {
        super();
        if (challenge == null) {
            throw new IllegalArgumentException("Challenge may not be null"); 
        }
        this.challenge = challenge;
    }

    /**
     * @see java.lang.Object#equals(Object)
     */
    public boolean equals(Object obj) {
        if (obj instanceof AuthSchemeBase) {
            return this.challenge.equals(((AuthSchemeBase) obj).challenge);
        } else {
            return super.equals(obj);
        }
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return this.challenge.hashCode();
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return this.challenge;
    }
}
