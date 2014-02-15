/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Cookie.java,v 1.4 2001/06/29 01:41:06 rwaldhoff Exp $
 * $Revision: 1.4 $
 * $Date: 2001-06-29 03:41:06 +0200 (Fri, 29 Jun 2001) $
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

import java.io.Serializable;

import java.util.Date;
import java.util.Enumeration;
import java.util.Vector;
import java.util.Locale;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.text.ParseException;

/**
 * This class represents an http cookie as specified in RFC 2109.
 *
 *
 * @author	B.C. Holmes
 * @author <a href="mailto:jericho@thinkfree.com">Park, Sung-Gu</a>
 * @author <a href="mailto:dsale@us.britannica.com">Doug Sale</a>
 */

public class Cookie extends NameValuePair implements Serializable {
    //
    // member variables
    //
    protected String  m_comment;
    protected String  m_domain;
    protected Date    m_expiryDate;
    protected String  m_path;
    protected boolean m_secure;

    // FIXME: JServ doesn't appear to explicitly set the version
    protected int     m_version = 1;


    /**
     * Create a cookie.
     *
     * @param name    the cookie name
     * @param value   the cookie value
     * @param domain  the host this cookie will be sent to
     * @param path    the path prefix for which this cookie will be sent
     * @param maxAge  the Date this cookie expires, null if the cookie
     *                expires at the end of the session
     * @param secure  if true this cookie will only be over secure connections
     * @exception NullPointerException if <var>name</var>, <var>value</var> or
     *                                 <var>domain</var> is null
     * @since V0.3-1
     */
    public Cookie(String domain, String name, String value) {
        super(name, value);
        if (name == null)   throw new NullPointerException("missing name");
        if (value == null)  throw new NullPointerException("missing value");
        if (domain == null) throw new NullPointerException("missing domain");

        this.setDomain(domain);
    }

    /**
     * Returns the comment describing the purpose of this cookie, or
     * null if no such comment has been defined.
     *
     * @see #setComment(String)
     */
    public String getComment() {
        return m_comment;
    }

    /**
     * If a user agent (web browser) presents this cookie to a user, the
     * cookie's purpose will be described using this comment.
     *
     * @see #getComment()
     */
    public void setComment(String comment) {
        m_comment = comment;
    }

    /**
     * @return the expiry date of this cookie, or null if none set.
     */
    public Date getExpiryDate() {
        return m_expiryDate;
    }

    /**
     * Expires and Max-Age
     *
     * <p>Netscape's original proposal defined an Expires header that took
     * a date value in a fixed-length variant format in place of Max-Age:
     *
     * Wdy, DD-Mon-YY HH:MM:SS GMT
     *
     * Note that the Expires date format contains embedded spaces, and that
     * "old" cookies did not have quotes around values.  Clients that
     * implement to this specification should be aware of "old" cookies and
     * Expires.
     *
     * @param expiryDate the expires date.
     */
    public void setExpiryDate (Date expiryDate) {
        m_expiryDate = expiryDate;
    }


    /**
     * @return true if the cookie should be discarded at the end of the
     *         session; false otherwise
     */
    public boolean isToBeDiscarded() {
        return (m_expiryDate != null);
    }


    /**
     * Returns the domain of this cookie.
     *
     * @see #setDomain(String)
     */
    public String getDomain() {
        return m_domain;
    }

    /**
     * This cookie should be presented only to hosts satisfying this domain
     * name pattern.  Read RFC 2109 for specific details of the syntax.
     * Briefly, a domain name name begins with a dot (".foo.com") and means
     * that hosts in that DNS zone ("www.foo.com", but not "a.b.foo.com")
     * should see the cookie.  By default, cookies are only returned to
     * the host which saved them.
     *
     * @see #getDomain()
     */
    public void setDomain(String domain) {
        int ndx = domain.indexOf(":");
        if (ndx != -1) {
          domain = domain.substring(0, ndx);
        }
        m_domain = domain.toLowerCase();
    }


    /**
     * Return the path this cookie is associated with.
     */
    public String getPath() {
        return m_path;
    }

    /**
     * This cookie should be presented only with requests beginning with this URL.
     * Read RFC 2109 for a specification of the default behaviour. Basically, URLs
     * in the same "directory" as the one which set the cookie, and in subdirectories,
     * can all see the cookie unless a different path is set.
     */
    public void setPath(String path) {
        m_path = path;
    }

    /**
     * Return whether this cookie should only be sent over secure connections.
     */
    public boolean getSecure() {
        return m_secure;
    }

    /**
     * Indicates to the user agent that the cookie should only be sent
     * using a secure protocol (https).  This should only be set when
     * the cookie's originating server used a secure protocol to set the
     * cookie's value.
     *
     * @see #getSecure()
     */
    public void setSecure (boolean secure) {
        m_secure = secure;
    }


    public int getVersion() {
        return m_version;
    }

    public void setVersion(int version) {
        m_version = version;
    }

    /**
     * @return true if this cookie has expired
     */
    public boolean isExpired() {
        return (m_expiryDate != null  &&
            m_expiryDate.getTime() <= System.currentTimeMillis());
    }


    /**
     * Hash up name, path and domain into new hash.
     */
    public int hashCode() {
        return (super.hashCode() + m_path.hashCode() + m_domain.hashCode());
    }


    /**
     * Two cookies match if the name, path and domain match.
     */
    public boolean equals(Object obj) {
        if ((obj != null) && (obj instanceof Cookie)) {
            Cookie other = (Cookie) obj;
            return  (this.getName().equals(other.getName())  &&
                     this.m_path.equals(other.m_path)  &&
                     this.m_domain.equals(other.m_domain));
        }
        return false;
    }


    /**
     * @return a string suitable for sending in a Cookie header.
     */
    public String toExternalForm() {
        String string = getName() + "=" + getValue();
        if (m_path != null) {
            string += "; $Path=" + m_path;
        }
        string += "; $Domain=" + m_domain;

        return string;
    }

    public static Header createCookieHeader(String domain,
            String path, Vector cookies) {

        // This code was allowing port values in the domain.  This is not part
        // of RFC2109.
        //
        // As per RFC2109 (from Section 2 - Terminology):
        //
        //    The terms request-host and request-URI refer to the values the client
        //    would send to the server as, respectively, the host (but not port)
        //    and abs_path portions of the absoluteURI (http_URL) of the HTTP
        //    request line.
        //
        // RFC2965 includes ports in cookie-sending determination, but only
        // when the cookie is received via a 'Set-Cookie2' header.
        //
        // Since this code doesn't support RFC2965, ports have been removed
        // from domains before checking mathces.
        //
        // removing port from domain
        int ndx = domain.indexOf(":");
        if (ndx != -1) {
            domain = domain.substring(0, ndx);
        }
        domain = domain.toLowerCase();

        StringBuffer value = new StringBuffer("$Version=1");

        // FIXME: cookies are supposed to be ordered with "better"
        //        matches first
        Date now = new Date();
        for (Enumeration e = cookies.elements(); e.hasMoreElements(); ) {
            Cookie cookie = (Cookie) e.nextElement();
            if ((cookie.getExpiryDate() == null || cookie.getExpiryDate().after(now)) && // only add the cookie if it hasn't yet expired
                domain.endsWith(cookie.getDomain()) &&                                   // and the domain pattern matches
                ((cookie.getPath() == null) ||                                           // and the path is null or matching
                 (path.startsWith(cookie.getPath())))) {
                value.append(";");
                value.append(cookie.toExternalForm());
            }
        }
        return new Header("Cookie", value.toString());
    }

    public String toString() {
        String string = toExternalForm();
        if (m_secure) {
            string += "; secure";
        }
        return string;
    }

    /**
      * Parses the Set-Cookie header into an array of Cookies.
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
      * @param domain the domain
      * @param setCookie the Set-Cookie header received from the server
      * @return an array of Cookies as parsed from the Set-Cookie header
      * @exception HttpException if an error occurs during parsing
      */
   public static Cookie[] parse(String domain, Header setCookie)
            throws HttpException {

        HeaderElement[] headerElements =
            HeaderElement.parse(setCookie.getValue());

        Cookie[] cookies = new Cookie[headerElements.length];
        int index = 0;
        for (int i = 0; i < headerElements.length; i++) {

            if (headerElements[i].getValue() == null)
                throw new HttpException(
                        "Bad Set-Cookie header: " + setCookie.getValue() +
                        "\nMissing value " + "for cookie '" +
                        headerElements[i].getName() + "'");

            Cookie cookie = new Cookie(domain,
                                       headerElements[i].getName(),
                                       headerElements[i].getValue());

            // cycle through the parameters
            NameValuePair[] parameters = headerElements[i].getParameters();
            // could be null. In case only a header element and no parameters.
            if (parameters == null) {
                cookies[index++] = cookie;
                // fix me, should be directory of the request, not root dir
                cookie.setPath("/");
                // go to the next header element.
                continue;
            }
            boolean discard_set = false, secure_set = false;
            for (int j = 0; j < parameters.length; j++) {
                String name = parameters[j].getName().toLowerCase();

                // check for required value parts
                if ( (name.equals("version") || name.equals("max-age") ||
                      name.equals("domain") || name.equals("path") ||
                      name.equals("comment") || name.equals("expires")) &&
                      parameters[j].getValue() == null) {
                    throw new HttpException(
                        "Bad Set-Cookie header: " + setCookie.getValue() +
                        "\nMissing value for " +
                        parameters[j].getName() +
                        " attribute in cookie '" +
                        headerElements[i].getName() + "'");
                }

                if (name.equals("version")) {
                    try {
                       cookie.setVersion(
                           Integer.parseInt(parameters[j].getValue()));
                    } catch (NumberFormatException nfe) {
                        throw new HttpException(
                                "Bad Set-Cookie header: " +
                                setCookie.getValue() + "\nVersion '" +
                                parameters[j].getValue() + "' not a number");
                    }
                } else if (name.equals("path")) {
                    cookie.setPath(parameters[j].getValue());
                } else if (name.equals("domain")) {
                    String d = parameters[j].getValue().toLowerCase();
                    // add leading dot if not present and if domain is
                    // not the full host name
                    if (d.charAt(0) != '.' && !d.equals(domain))
                        cookie.setDomain("." + d);
                    else
                        cookie.setDomain(d);
                } else if (name.equals("max-age")) {
                    int age;
                    try {
                        age = Integer.parseInt(parameters[j].getValue());
                    } catch (NumberFormatException e) {
                        throw new HttpException(
                                "Bad Set-Cookie header: " +
                                setCookie.getValue() + "\nMax-Age '" +
                                parameters[j].getValue() + "' not a number");
                    }
                    cookie.setExpiryDate(new Date(System.currentTimeMillis() +
                            age * 1000L));
                } else if (name.equals("secure")) {
                    cookie.setSecure(true);
                } else if (name.equals("comment")) {
                    cookie.setComment(parameters[j].getValue());
                } else if (name.equals("expires")) {
                    /*
                     * In the RFC 2109 for the cookies,
                     * the Expires date format is "Wdy, DD-Mon-YY HH:MM:SS GMT".
                     * There might be one more?  Wdy, DD-Mon-YYYY HH:MM:SS GMT
                     */
                    try {
                        // RFC 1123, 822, Date and time specification is English.
                        DateFormat formatter = new SimpleDateFormat
                            ("EEE, dd-MMM-yyyy HH:mm:ss z", Locale.US);
                        String expiryDate = parameters[j].getValue();
                        Date date = formatter.parse(expiryDate);
                        cookie.setExpiryDate(date);
                    } catch (ParseException e) {
                    }
                }
            }

            // check version
            if (cookie.getVersion() != 1) {
                throw new HttpException(
                        "Bad Set-Cookie header: " + setCookie.getValue() +
                        "\nIllegal Version attribute");
            }

            // security check... we musn't allow the server to give us an
            // invalid domain scope

            // domain must be either .local or must contain at least two dots
            if (!cookie.getDomain().equals("localhost")) {

                // Not required to have at least two dots.  RFC 2965.
                // A Set-Cookie2 with Domain=ajax.com will be accepted.

                // domain must domain match host
                if (!domain.endsWith(cookie.getDomain())){
                    throw new HttpException(
                        "Bad Set-Cookie header: " + setCookie.getValue() +
                        "\nIllegal domain attribute" + cookie.getDomain());
                }

                // host minus domain may not contain any dots
                if (domain.substring(0,
                        domain.length() -
                        cookie.getDomain().length()).indexOf('.') != -1) {
                    throw new HttpException(
                        "Bad Set-Cookie header: " + setCookie.getValue() +
                        "\nIllegal domain attribute" + cookie.getDomain());
                }
            }

            // looks ok
            cookies[index++] = cookie;
        }

        return cookies;
    }
}

