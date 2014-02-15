/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/cookie/CookieSpecBase.java,v 1.7 2003/01/23 22:48:06 jsdever Exp $
 * $Revision: 1.7 $
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

import java.util.List;
import java.util.LinkedList;
import java.util.Date;
import java.util.Locale;   
import java.text.DateFormat; 
import java.text.ParseException; 
import java.text.SimpleDateFormat;  
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HeaderElement;
import org.apache.commons.httpclient.Cookie;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * 
 * Cookie management functions shared by all specification.
 *
 * @author  B.C. Holmes
 * @author <a href="mailto:jericho@thinkfree.com">Park, Sung-Gu</a>
 * @author <a href="mailto:dsale@us.britannica.com">Doug Sale</a>
 * @author Rod Waldhoff
 * @author dIon Gillard
 * @author Sean C. Sullivan
 * @author <a href="mailto:JEvans@Cyveillance.com">John Evans</a>
 * @author Marc A. Saegesser
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * 
 * @since 2.0 
 */

public class CookieSpecBase implements CookieSpec
{
    /** Log object */
    protected static final Log log = LogFactory.getLog(CookieSpec.class);

    /** List of valid date formats for the "expires" cookie attribute. */
    private static final DateFormat[] expiryFormats = {
       // RFC 1123, 822, Date and time specification is English.
       new SimpleDateFormat("EEE, dd-MMM-yy HH:mm:ss z", Locale.US),
       new SimpleDateFormat("EEE, dd-MMM-yyyy HH:mm:ss z", Locale.US),
       new SimpleDateFormat("EEE dd-MMM-yy HH:mm:ss z", Locale.US),
       new SimpleDateFormat("EEE dd-MMM-yyyy HH:mm:ss z", Locale.US),
       new SimpleDateFormat("EEE dd MMM yy HH:mm:ss z", Locale.US),
       new SimpleDateFormat("EEE dd MMM yyyy HH:mm:ss z", Locale.US),
       new SimpleDateFormat("EEE, dd MMM yy HH:mm:ss z", Locale.US),
       new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US),
       new SimpleDateFormat("EEE, dd-MMM-yyyy HH-mm-ss z", Locale.US),
       new SimpleDateFormat("EEE dd-MMM-yyyy HH-mm-ss z", Locale.US)
    };

    /** Default constructor */

    public CookieSpecBase()
    {
        super();
    }


    /**
      * Parses the Set-Cookie value into an array of <tt>Cookie</tt>s.
      *
      * <P>The syntax for the Set-Cookie response header is:
      *
      * <PRE>
      * set-cookie      =    "Set-Cookie:" cookies
      * cookies         =    1#cookie
      * cookie          =    NAME "=" VALUE * (";" cookie-av)
      * NAME            =    attr
      * VALUE           =    value
      * cookie-av       =    "Comment" "=" value
      *                 |    "Domain" "=" value
      *                 |    "Max-Age" "=" value
      *                 |    "Path" "=" value
      *                 |    "Secure"
      *                 |    "Version" "=" 1*DIGIT
      * </PRE>
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
      throws MalformedCookieException
    {
        log.trace("enter CookieSpecBase.parse(String, port, path, boolean, Header)");

        if(host == null)
        {
            throw new IllegalArgumentException("Host of origin may not be null");
        }
        if(host.trim().equals(""))
        {
            throw new IllegalArgumentException("Host of origin may not be blank");
        }
        if(port < 0)
        {
            throw new IllegalArgumentException("Invalid port: " + port);
        }
        if(path == null)
        {
            throw new IllegalArgumentException("Path of origin may not be null.");
        }
        if(header == null)
        {
            throw new IllegalArgumentException("Header may not be null.");
        }

        if (path.trim().equals("")) {
            path = PATH_DELIM;
        }
        host = host.toLowerCase();
    
        HeaderElement[] headerElements = null;
        try
        {
            headerElements = HeaderElement.parse(header);
        }
        catch(HttpException e)
        {
            throw new MalformedCookieException(e.getMessage());
        } 
    
        String defaultPath = path;    
        int lastSlashIndex = defaultPath.lastIndexOf(PATH_DELIM);
        if(lastSlashIndex > 0)
        {
            defaultPath = defaultPath.substring(0, lastSlashIndex);
        }
        
        Cookie[] cookies = new Cookie[headerElements.length];

        for (int i = 0; i < headerElements.length; i++) {

            HeaderElement headerelement = headerElements[i];
            Cookie cookie = new Cookie(host,
                                       headerelement.getName(),
                                       headerelement.getValue(),
                                       defaultPath, 
                                       null,
                                       false);

            // cycle through the parameters
            NameValuePair[] parameters = headerelement.getParameters();
            // could be null. In case only a header element and no parameters.
            if (parameters != null) {

                for (int j = 0; j < parameters.length; j++)
                {
                    parseAttribute(parameters[j], cookie);
                }
            }
            cookies[i] = cookie;
        }
        return cookies;
    }


    /**
      * Parse the <tt>"Set-Cookie"</tt> {@link Header} into an array of {@link Cookie}s.
      *
      * <P>The syntax for the Set-Cookie response header is:
      *
      * <PRE>
      * set-cookie      =    "Set-Cookie:" cookies
      * cookies         =    1#cookie
      * cookie          =    NAME "=" VALUE * (";" cookie-av)
      * NAME            =    attr
      * VALUE           =    value
      * cookie-av       =    "Comment" "=" value
      *                 |    "Domain" "=" value
      *                 |    "Max-Age" "=" value
      *                 |    "Path" "=" value
      *                 |    "Secure"
      *                 |    "Version" "=" 1*DIGIT
      * </PRE>
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
      throws MalformedCookieException
    {
        log.trace("enter CookieSpecBase.parse(String, port, path, boolean, String)");
        if(header == null)
        {
            throw new IllegalArgumentException("Header may not be null.");
        }
        return parse(host, port, path, secure, header.getValue());
    }


    /**
      * Parse the cookie attribute and update the corresponsing {@link Cookie} properties.
      *
      * @param attribute {@link HeaderElement} cookie attribute from the <tt>Set-Cookie</tt>
      * @param cookie {@link Cookie} to be updated
      * @throws MalformedCookieException if an exception occurs during parsing
      * @throws java.lang.IllegalArgumentException if an input parameter is illegal
      */

    public void parseAttribute(final NameValuePair attribute, final Cookie cookie)
      throws MalformedCookieException
    {
        if(attribute == null)
        {
            throw new IllegalArgumentException("Attribute may not be null.");
        }
        if(cookie == null)
        {
            throw new IllegalArgumentException("Cookie may not be null.");
        }
        String param_name = attribute.getName().toLowerCase();
        String param_value = attribute.getValue();

        if (param_name.equals("path")) {

            if (param_value == null) {
                throw new MalformedCookieException("Missing value for path attribute");
            }
            if (param_value.trim().equals("")) {
                throw new MalformedCookieException("Blank value for path attribute");
            }
            cookie.setPath(param_value);
            cookie.setPathAttributeSpecified(true);

        } 
        else if (param_name.equals("domain")) {

            if (param_value == null) {
                throw new MalformedCookieException("Missing value for domain attribute");
            }
            if (param_value.trim().equals("")) {
                throw new MalformedCookieException("Blank value for domain attribute");
            }
            cookie.setDomain(param_value);
            cookie.setDomainAttributeSpecified(true);

        } 
        else if (param_name.equals("max-age")) {

            if (param_value == null) {
                throw new MalformedCookieException("Missing value for max-age attribute");
            }
            int age;
            try {
                age = Integer.parseInt(param_value);
            } catch (NumberFormatException e) {
                throw new MalformedCookieException( "Invalid max-age attribute: " + e.getMessage());
            }
            cookie.setExpiryDate(new Date(System.currentTimeMillis() +
                    age * 1000L));

        } 
        else if (param_name.equals("secure")) {

            cookie.setSecure(true);

        } 
        else if (param_name.equals("comment")) {

            cookie.setComment(param_value);

        } 
        else if (param_name.equals("expires")) {

            if (param_value == null) {
                throw new MalformedCookieException("Missing value for expires attribute");
            }
            boolean set = false;
            // trim single quotes around expiry if present
            // see http://nagoya.apache.org/bugzilla/show_bug.cgi?id=5279
            if(param_value.length() > 1 &&
                    param_value.startsWith("'") &&
                    param_value.endsWith("'")) {
                param_value = param_value.substring(1,param_value.length()-1);
            }

            for(int k=0;k<expiryFormats.length;k++) {

                try {
                    Date date = expiryFormats[k].parse(param_value);
                    cookie.setExpiryDate(date);
                    set = true;
                    break;
                } catch (ParseException e) {
                    //Ignore and move on
                }
            }
            if(!set) {
                throw new MalformedCookieException("Unable to parse expiration date parameter: " + param_value);
            }
        }
        else {
            if (log.isWarnEnabled())
            {
                log.warn("Unrecognized cookie attribute: " + attribute.toString());
            }
        }
    }

    
    /**
      * Performs most common {@link Cookie} validation
      *
      * @param host the host from which the {@link Cookie} was received
      * @param port the port from which the {@link Cookie} was received
      * @param path the path from which the {@link Cookie} was received
      * @param secure <tt>true</tt> when the {@link Cookie} was received using a secure connection
      * @throws MalformedCookieException if an exception occurs during validation
      * @throws java.lang.IllegalArgumentException if an input parameter is illegal
      */
    
    public void validate(String host, int port, String path, boolean secure, final Cookie cookie) throws MalformedCookieException
    {
        log.trace("enter CookieSpecBase.validate(String, port, path, boolean, Cookie)");
        if(host == null)
        {
            throw new IllegalArgumentException("Host of origin may not be null");
        }
        if(host.trim().equals(""))
        {
            throw new IllegalArgumentException("Host of origin may not be blank");
        }
        if(port < 0)
        {
            throw new IllegalArgumentException("Invalid port: " + port);
        }
        if(path == null)
        {
            throw new IllegalArgumentException("Path of origin may not be null.");
        }
        if (path.trim().equals("")) {
            path = PATH_DELIM;
        }
        host = host.toLowerCase();
        // check version
        if (cookie.getVersion() < 0) 
        {
            throw new MalformedCookieException( "Illegal version number " + cookie.getValue());
        }

        // security check... we musn't allow the server to give us an
        // invalid domain scope

        // Validate the cookies domain attribute.  NOTE:  Domains without any dots are
        // allowed to support hosts on private LANs that don't have DNS names.  Since
        // they have no dots, to domain-match the request-host and domain must be identical
        // for the cookie to sent back to the origin-server.
        if (host.indexOf(".") >= 0)
        {
            // Not required to have at least two dots.  RFC 2965.
            // A Set-Cookie2 with Domain=ajax.com will be accepted.

            // domain must match host
            if (!host.endsWith(cookie.getDomain()))
            {
                throw new MalformedCookieException(
                    "Illegal domain attribute \"" + cookie.getDomain() + "\". Domain of origin: \"" + host + "\"");
            }
        }

        // another security check... we musn't allow the server to give us a
        // cookie that doesn't match this path

        if(!path.startsWith(cookie.getPath()))
        {
            throw new MalformedCookieException(
                    "Illegal path attribute \"" + cookie.getPath() + "\". Path of origin: \"" + path + "\"");
        }
    }


    /**
     * Return <tt>true</tt> if the cookie should be submitted with a request with
     * given attributes, <tt>false</tt> otherwise.
     * @param host the host to which the request is being submitted
     * @param port the port to which the request is being submitted (ignored)
     * @param path the path to which the request is being submitted
     * @param secure <tt>true</tt> if the request is using a secure connection
     * @param {@link Cookie} to be matched
     * @return true if the cookie matches the criterium
     */

    public boolean match(String host, int port, String path, boolean secure, final Cookie cookie) 
    {
        log.trace("enter CookieSpecBase.match(String, int, String, boolean, Cookie");
        if(host == null)
        {
            throw new IllegalArgumentException("Host of origin may not be null");
        }
        if(host.trim().equals(""))
        {
            throw new IllegalArgumentException("Host of origin may not be blank");
        }
        if(port < 0)
        {
            throw new IllegalArgumentException("Invalid port: " + port);
        }
        if(path == null)
        {
            throw new IllegalArgumentException("Path of origin may not be null.");
        }
        if(cookie == null)
        {
            throw new IllegalArgumentException("Cookie may not be null");
        }
        if (path.trim().equals("")) {
            path = PATH_DELIM;
        }
        host = host.toLowerCase();
        if (cookie.getDomain() == null) {
            log.warn("Invalid cookie state: domain not specified");
            return false;
        }
        if (cookie.getPath() == null) {
            log.warn("Invalid cookie state: path not specified");
            return false;
        }
        
        return (
                (cookie.getExpiryDate() == null || 
                 cookie.getExpiryDate().after(new Date())) && // only add the cookie if it hasn't yet expired
                (domainMatch(host, cookie.getDomain())) &&    // and the domain pattern matches
                (pathMatch(path, cookie.getPath())) &&        // and the path is null or matching
                (cookie.getSecure() ? secure : true)          // and if the secure flag is set, only if the request is actually secure
               );
    }

    /**
     * Performs a domain-match as described in RFC2109.
     */
    private static boolean domainMatch(String host, String domain)
    {
        boolean match = host.equals(domain) ||
                        (domain.startsWith(".") && host.endsWith(domain));

        return match;
    }

    /**
     * Performs a path-match slightly smarter than a straight-forward startsWith check.
     */
    private static boolean pathMatch(final String path, final String topmostPath)
    {
        boolean match = path.startsWith( topmostPath );
        
        // if there is a match and these values are not exactly the same we have
        // to make sure we're not matcing "/foobar" and "/foo"
        if ( match && path.length() != topmostPath.length() ) {
            if (!topmostPath.endsWith(PATH_DELIM)) {
                match = (path.charAt(topmostPath.length()) == PATH_DELIM_CHAR);
            }
        }
        return match;
    }

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

    public Cookie[] match(String host, int port, String path, boolean secure, final Cookie cookies[])
    {
        log.trace("enter CookieSpecBase.match(String, int, String, boolean, Cookie[])");

        if(host == null)
        {
            throw new IllegalArgumentException("Host of origin may not be null");
        }
        if(host.trim().equals(""))
        {
            throw new IllegalArgumentException("Host of origin may not be blank");
        }
        if(port < 0)
        {
            throw new IllegalArgumentException("Invalid port: " + port);
        }
        if(path == null)
        {
            throw new IllegalArgumentException("Path of origin may not be null.");
        }
        if(cookies == null)
        {
            throw new IllegalArgumentException("Cookie array may not be null");
        }
        if (path.trim().equals("")) {
            path = PATH_DELIM;
        }
        host = host.toLowerCase();

        StringBuffer value = new StringBuffer();

        if (cookies.length <= 0) {
            return null;
        }
        List matching = new LinkedList();
        for(int i=0;i<cookies.length;i++)
        {
            if(match(host, port, path, secure, cookies[i]))
            {
                addInPathOrder(matching, cookies[i]);
            }
        }
        return (Cookie[])matching.toArray(new Cookie[matching.size()]);
    }


    /**
     * Adds the given cookie into the given list in descending path order.  That is, 
     * more specific path to least specific paths.  This may not be the fastest
     * algorythm, but it'll work OK for the small number of cookies we're 
     * generally dealing with.
     *
     * @param list - the list to add the cookie to
     * @param addCookie - the Cookie to add to list
     */
    private static void addInPathOrder(List list, Cookie addCookie)
    {
        int i = 0;

        for(i=0;i<list.size();i++){
            Cookie c = (Cookie)list.get(i);
            if(addCookie.compare(addCookie, c) > 0){
                break;
            }
        }
        list.add(i, addCookie);
    }

    /**
     * Return a string suitable for sending in a <tt>"Cookie"</tt> header
     * @param a {@link Cookie} to be formatted as string
     * @return a string suitable for sending in a <tt>"Cookie"</tt> header.
     */

    public String formatCookie(Cookie cookie)
    {
        log.trace("enter CookieSpecBase.formatCookie(Cookie)");
        if(cookie == null)
        {
            throw new IllegalArgumentException("Cookie may not be null");
        }
        StringBuffer buf = new StringBuffer();
        buf.append(cookie.getName()).append("=").append(cookie.getValue());
        return buf.toString();
    }

    /**
     * Create a <tt>"Cookie"</tt> header value containing all {@link Cookie}s in <i>cookies</i>
     * suitable for sending in a <tt>"Cookie"</tt> header
     * @param an array of {@link Cookie}s to be formatted
     * @return a string suitable for sending in a Cookie header.
     * @throws java.lang.IllegalArgumentException if an input parameter is illegal
     */

    public String formatCookies(Cookie[] cookies)
      throws IllegalArgumentException
    {
        log.trace("enter CookieSpecBase.formatCookies(Cookie[])");
        if(cookies == null)
        {
            throw new IllegalArgumentException("Cookie array may not be null");
        }
        if(cookies.length == 0)
        {
            throw new IllegalArgumentException("Cookie array may not be empty");
        }

        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < cookies.length; i++)
        {
            if (i > 0)
            {
                buffer.append("; ");
            }
            buffer.append(formatCookie(cookies[i]));
        }
        return buffer.toString();
    }


    /**
     * Create a <tt>"Cookie"</tt> {@link Header} containing all {@link Cookie}s in <i>cookies</i>.
     * @param an array of {@link Cookie}s to be formatted as a <tt>"Cookie"</tt> header
     * @return a <tt>"Cookie"</tt> {@link Header}.
     * @throws java.lang.IllegalArgumentException if an input parameter is illegal
     */

    public Header formatCookieHeader(Cookie[] cookies)
    {
        log.trace("enter CookieSpecBase.formatCookieHeader(Cookie[])");
        return new Header("Cookie", formatCookies(cookies));
    }


    /**
     * Create a <tt>"Cookie"</tt> {@link Header} containing the {@link Cookie}.
     * @param <tt>Cookie</tt>s to be formatted as a <tt>Cookie</tt> header
     * @return a Cookie header.
     * @throws java.lang.IllegalArgumentException if an input parameter is illegal
     */

    public Header formatCookieHeader(Cookie cookie)
    {
        log.trace("enter CookieSpecBase.formatCookieHeader(Cookie)");
        return new Header("Cookie", formatCookie(cookie));
    }

}
