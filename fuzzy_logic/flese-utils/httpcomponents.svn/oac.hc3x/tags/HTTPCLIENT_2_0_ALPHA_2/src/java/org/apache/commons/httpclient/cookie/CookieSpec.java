/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/cookie/CookieSpec.java,v 1.4 2003/01/23 22:48:06 jsdever Exp $
 * $Revision: 1.4 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
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

package org.apache.commons.httpclient.cookie;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.Cookie;

/**
 * <P>Cookie management specification must define
 * <ul>
 *   <li> rules of parsing "Set-Cookie" header
 *   <li> rules of validation of parsed cookies
 *   <li>  formatting of "Cookie" header 
 * </ul>
 * for a given host, port and path of origin
 * 
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 *
 * @since 2.0
 */

public interface CookieSpec
{    
    /** Path delimiter */
    public static final String PATH_DELIM = "/";
    /** Path delimiting charachter */
    public static final char   PATH_DELIM_CHAR = PATH_DELIM.charAt(0);

    /**
      * Parse the <tt>"Set-Cookie"</tt> header value into an array of {@link Cookie}s.
      *
      * @param host the host from which the <tt>Set-Cookie</tt> value was received
      * @param port the port from which the <tt>Set-Cookie</tt> value was received
      * @param path the path from which the <tt>Set-Cookie</tt> value was received
      * @param secure <tt>true</tt> when the <tt>Set-Cookie</tt> value was received over secure conection
      * @param header the <tt>Set-Cookie</tt> received from the server
      * @return an array of <tt>Cookie</tt>s parsed from the Set-Cookie value
      * @throws MalformedCookieException if an exception occurs during parsing
      * @throws java.lang.IllegalArgumentException if an input parameter is illegal
      */

    public Cookie[] parse(String host, int port, String path, boolean secure, final String header)
      throws MalformedCookieException;

    /**
      * Parse the <tt>"Set-Cookie"</tt> {@link Header} into an array of {@link Cookie}s.
      *
      * @param host the host from which the <tt>Set-Cookie</tt> header was received
      * @param port the port from which the <tt>Set-Cookie</tt> header was received
      * @param path the path from which the <tt>Set-Cookie</tt> header was received
      * @param secure <tt>true</tt> when the <tt>Set-Cookie</tt> header was received over secure conection
      * @param header the <tt>Set-Cookie</tt> received from the server
      * @return an array of <tt>Cookie</tt>s parsed from the <tt>"Set-Cookie"</tt> header
      * @throws MalformedCookieException if an exception occurs during parsing
      * @throws java.lang.IllegalArgumentException if an input parameter is illegal
      */

    public Cookie[] parse(String host, int port, String path, boolean secure, final Header header)
      throws MalformedCookieException;

    /**
      * Parse the cookie attribute and update the corresponsing {@link Cookie} properties.
      *
      * @param attribute {@link NameValuePair} cookie attribute from the <tt>Set-Cookie</tt>
      * @param cookie {@link Cookie} to be updated
      * @throws MalformedCookieException if an exception occurs during parsing
      * @throws java.lang.IllegalArgumentException if an input parameter is illegal
      */

    public void parseAttribute(final NameValuePair attribute, final Cookie cookie)
      throws MalformedCookieException;

    /**
      * Perform {@link Cookie} validation accoding to validation rules defined by the cookie specification
      *
      * @param host the host from which the {@link Cookie} was received
      * @param port the port from which the {@link Cookie} was received
      * @param path the path from which the {@link Cookie} was received
      * @param secure <tt>true</tt> when the {@link Cookie} was received using a secure connection
      * @throws MalformedCookieException if an exception occurs during validation
      * @throws java.lang.IllegalArgumentException if an input parameter is illegal
      */

    public void validate(String host, int port, String path, boolean secure, final Cookie cookie) 
      throws MalformedCookieException;
    
    /**
     * Return <tt>true</tt> if the cookie should be submitted with a request with
     * given attributes, <tt>false</tt> otherwise.
     * @param host the host to which the request is being submitted
     * @param port the port to which the request is being submitted
     * @param path the path to which the request is being submitted
     * @param secure <tt>true</tt> if the request is using a secure connection
     * @param {@link Cookie} to be matched
     * @return true if the cookie matches the criterium
     */

    public boolean match(String host, int port, String path, boolean secure, final Cookie cookie);

    /**
     * Return an array of {@link Cookie}s that should be submitted with a request with
     * given attributes, <tt>false</tt> otherwise.
     * @param host the host to which the request is being submitted
     * @param port the port to which the request is being submitted (currenlty ignored)
     * @param path the path to which the request is being submitted
     * @param secure <tt>true</tt> if the request is using a secure protocol
     * @param an array of <tt>Cookie</tt>s to be matched
     * @return an array of <tt>Cookie</tt>s matching the criterium
     */

    public Cookie[] match(String host, int port, String path, boolean secure, final Cookie cookies[]);

    /**
     * Return a string suitable for sending in a <tt>"Cookie"</tt> header
     * @param a {@link Cookie} to be formatted as string
     * @return a string suitable for sending in a <tt>"Cookie"</tt> header.
     */

    public String formatCookie(Cookie cookie);

    /**
     * Create a <tt>"Cookie"</tt> header value containing all {@link Cookie}s in <i>cookies</i>
     * suitable for sending in a <tt>"Cookie"</tt> header
     * @param an array of {@link Cookie}s to be formatted
     * @return a string suitable for sending in a Cookie header.
     * @throws java.lang.IllegalArgumentException if an input parameter is illegal
     */

    public String formatCookies(Cookie[] cookies);
    
    /**
     * Create a <tt>"Cookie"</tt> {@link Header} containing all {@link Cookie}s in <i>cookies</i>.
     * @param an array of {@link Cookie}s to be formatted as a <tt>"Cookie"</tt> header
     * @return a <tt>"Cookie"</tt> {@link Header}.
     * @throws java.lang.IllegalArgumentException if an input parameter is illegal
     */

    public Header formatCookieHeader(Cookie[] cookies);

    /**
     * Create a <tt>"Cookie"</tt> {@link Header} containing the {@link Cookie}.
     * @param <tt>Cookie</tt>s to be formatted as a <tt>Cookie</tt> header
     * @return a Cookie header.
     * @throws java.lang.IllegalArgumentException if an input parameter is illegal
     */

    public Header formatCookieHeader(Cookie cookie);

}
