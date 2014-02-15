/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpURL.java,v 1.7 2003/01/23 22:47:47 jsdever Exp $
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

package org.apache.commons.httpclient;

/**
 * The HTTP URL.
 *
 * @author <a href="mailto:jericho at apache.org">Sung-Gu</a>
 */
public class HttpURL extends URI {

    // ----------------------------------------------------------- Constructors

    protected HttpURL() {
    }

    /**
     * Construct a HTTP URL as an escaped form of a character array.
     *
     * @param escaped the HTTP URL character sequence
     * @exception URIException
     * @throws NullPointerException if <code>escaped</code> is <code>null</code>
     */
    public HttpURL(char[] escaped) throws URIException {
        parseUriReference(new String(escaped), true);
        checkValid();
    }


    /**
     * Construct a HTTP URL from a given string.
     *
     * @param original the HTTP URL string
     * @exception URIException
     */
    public HttpURL(String original) throws URIException {
        parseUriReference(original, false);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param host the host string
     * @param path the path string
     * @exception URIException
     */
    public HttpURL(String host, String path) throws URIException {
        this(null, host, -1, path, null, null);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param host the host string
     * @param port the port number
     * @param path the path string
     * @exception URIException
     */
    public HttpURL(String host, int port, String path) throws URIException {
        this(null, host, port, path, null, null);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param host the host string
     * @param port the port number
     * @param path the path string
     * @param query the query string
     * @exception URIException
     */
    public HttpURL(String host, int port, String path, String query)
        throws URIException {

        this(null, host, port, path, query, null);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param user the user name
     * @param password his or her password
     * @param host the host string
     * @exception URIException
     */
    public HttpURL(String user, String password, String host)
        throws URIException {

        this((user == null) ? null : user +
                ((password == null) ? "" : ':' +  password),
                host, -1, null, null, null);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param user the user name
     * @param password his or her password
     * @param host the host string
     * @param port the port number
     * @exception URIException
     */
    public HttpURL(String user, String password, String host, int port)
        throws URIException {

        this((user == null) ? null : user +
                ((password == null) ? "" : ':' +  password),
                host, port, null, null, null);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param user the user name
     * @param password his or her password
     * @param host the host string
     * @param port the port number
     * @param path the path string
     * @exception URIException
     */
    public HttpURL(String user, String password, String host, int port,
            String path) throws URIException {

        this((user == null) ? null : user +
                ((password == null) ? "" : ':' +  password),
                host, port, path, null, null);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param user the user name
     * @param password his or her password
     * @param host the host string
     * @param port the port number
     * @param path the path string
     * @exception URIException
     */
    public HttpURL(String user, String password, String host, int port,
            String path, String query) throws URIException {

        this((user == null) ? null : user +
                ((password == null) ? "" : ':' + password),
                host, port, path, query, null);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param host the host string
     * @param path the path string
     * @param query the query string
     * @param fragment the fragment string
     * @exception URIException
     */
    public HttpURL(String host, String path, String query, String fragment)
        throws URIException {

        this(null, host, -1, path, query, fragment);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param userinfo the userinfo string
     * @param host the host string
     * @param path the path string
     * @param query the query string
     * @param fragment the fragment string
     * @exception URIException
     */
    public HttpURL(String userinfo, String host, String path, String query,
            String fragment) throws URIException {

        this(userinfo, host, -1, path, query, fragment);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param userinfo the userinfo string
     * @param host the host string
     * @param port the port number
     * @param path the path string
     * @exception URIException
     */
    public HttpURL(String userinfo, String host, int port, String path)
        throws URIException {

        this(userinfo, host, port, path, null, null);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param userinfo the userinfo string
     * @param host the host string
     * @param port the port number
     * @param path the path string
     * @param query the query string
     * @exception URIException
     */
    public HttpURL(String userinfo, String host, int port, String path,
            String query) throws URIException {

        this(userinfo, host, port, path, query, null);
        checkValid();
    }


    /**
     * Construct a HTTP URL from given components.
     *
     * @param userinfo the userinfo string
     * @param host the host string
     * @param port the port number
     * @param path the path string
     * @param query the query string
     * @param fragment the fragment string
     * @exception URIException
     */
    public HttpURL(String userinfo, String host, int port, String path,
            String query, String fragment) throws URIException {

        // validate and contruct the URI character sequence
        StringBuffer buff = new StringBuffer();
        if (userinfo != null || host != null || port != -1) {
            _scheme = _default_scheme; // in order to verify the own protocol
            buff.append(_default_scheme);
            buff.append("://");
            if (userinfo != null) {
                buff.append(userinfo);
                buff.append('@');
            }
            if (host != null) {
                buff.append(host);
                if (port != -1 || port != _default_port) {
                    buff.append(':');
                    buff.append(port);
                }
            }
        }
        if (path != null) {  // accept empty path
            if (scheme != null && !path.startsWith("/")) {
                throw new URIException(URIException.PARSING,
                        "abs_path requested");
            }
            buff.append(path);
        }
        if (query != null) {
            buff.append('?');
            buff.append(query);
        }
        if (fragment != null) {
            buff.append('#');
            buff.append(fragment);
        }
        parseUriReference(buff.toString(), false);
        checkValid();
    }


    /**
     * Construct a HTTP URL with a given relative URL string.
     *
     * @param base the base HttpURL
     * @param relative the relative HTTP URL string
     * @exception URIException
     */
    public HttpURL(HttpURL base, String relative) throws URIException {
        this(base, new HttpURL(relative));
    }


    /**
     * Construct a HTTP URL with a given relative URL.
     *
     * @param base the base HttpURL
     * @param relative the relative HttpURL
     * @exception URIException
     */
    public HttpURL(HttpURL base, HttpURL relative) throws URIException {
        super(base, relative);
        checkValid();
    }

    // -------------------------------------------------------------- Constants

    /**
     * Default scheme for HTTP URL.
     */
    public static final char[] _default_scheme = { 'h', 't', 't', 'p' };


    /**
     * Default port for HTTP URL.
     */
    public static final int _default_port = 80;


    /**
     * The serialVersionUID.
     */
    static final long serialVersionUID = -7158031098595039459L;

    // ------------------------------------------------------------- The scheme

    /**
     * Get the scheme.  You can get the scheme explicitly.
     *
     * @return the scheme
     */
    public char[] getRawScheme() {
        return (_scheme == null) ? null : _default_scheme;
    }


    /**
     * Get the scheme.  You can get the scheme explicitly.
     *
     * @return the scheme null if empty or undefined
     */
    public String getScheme() {
        return (_scheme == null) ? null : new String(_default_scheme);
    }

    // --------------------------------------------------------------- The port

    /**
     * Get the port number.
     */
    public int getPort() {
        return (_port == -1) ? _default_port : _port;
    }

    // ----------------------------------------------------------- The userinfo

    /**
     * Set the raw-escaped user and password.
     *
     * @param escapedUser the raw-escaped user
     * @param escapedPassword the raw-escaped password; could be null
     * @exception URIException escaped user not valid or user required; escaped
     * password not valid or username missed
     */
    public void setRawUserinfo(char[] escapedUser, char[] escapedPassword)
        throws URIException {

        if (escapedUser == null || escapedUser.length == 0)
            throw new URIException(URIException.PARSING, "user required");
        if (!validate(escapedUser, within_userinfo) ||
                ((escapedPassword != null) &&
                 !validate(escapedPassword, within_userinfo)))
            throw new URIException(URIException.ESCAPING,
                    "escaped userinfo not valid");
        String username = new String(escapedUser);
        String password = (escapedPassword == null) ? null :
            new String(escapedPassword);
        String userinfo = username + ((password == null) ? "" : ":" + password);
        String hostname = new String(getRawHost());
        String hostport = (_port == -1) ? hostname : hostname + _port;
        String authority = userinfo + "@" + hostport;
        _userinfo = userinfo.toCharArray();
        _authority = authority.toCharArray();
        setURI();
    }


    /**
     * Set the raw-escaped user and password.
     *
     * @param escapedUser the escaped user
     * @param escapedPassword the escaped password; could be null
     * @exception URIException escaped user not valid or user required; escaped
     * password not valid or username missed
     * @throws NullPointerException null user
     */
    public void setEscapedUserinfo(String escapedUser, String escapedPassword)
        throws URIException {

        setRawUserinfo(escapedUser.toCharArray(), (escapedPassword == null) ?
            null : escapedPassword.toCharArray());
    }


    /**
     * Set the user and password.
     *
     * @param user the user
     * @param password the password; could be null
     * @exception URIException encoding error or username missed
     * @throws NullPointerException null user
     */
    public void setUserinfo(String user, String password) throws URIException {
        setRawUserinfo(encode(user, within_userinfo), (password == null) ?
                null : encode(password, within_userinfo));
    }


    /**
     * Set the raw-escaped user.
     *
     * @param the raw-escaped user
     * @exception URIException escaped user not valid or user required
     */
    public void setRawUser(char[] escapedUser) throws URIException {
        if (escapedUser == null || escapedUser.length == 0)
            throw new URIException(URIException.PARSING, "user required");
        if (!validate(escapedUser, within_userinfo))
            throw new URIException(URIException.ESCAPING,
                    "escaped user not valid");
        String username = new String(escapedUser);
        String password = new String(getRawPassword());
        String userinfo = username + ((password == null) ? "" : ":" + password);
        String hostname = new String(getRawHost());
        String hostport = (_port == -1) ? hostname : hostname + _port;
        String authority = userinfo + "@" + hostport;
        _userinfo = userinfo.toCharArray();
        _authority = authority.toCharArray();
        setURI();
    }


    /**
     * Set the escaped user string.
     *
     * @param escapedUser the escaped user string
     * @exception URIException escaped user not valid
     * @throws NullPointerException null user
     */
    public void setEscapedUser(String escapedUser) throws URIException {
        setRawUser(escapedUser.toCharArray());
    }


    /**
     * Set the user string.
     *
     * @param user the user string
     * @exception URIException user encoding error
     * @throws NullPointerException null user
     */
    public void setUser(String user) throws URIException {
        setRawUser(encode(user, allowed_within_userinfo));
    }


    /**
     * Get the raw-escaped user.
     *
     * @return the raw-escaped user
     */
    public char[] getRawUser() {
        if (_userinfo == null || _userinfo.length == 0) {
            return null;
        }
        int to = indexFirstOf(_userinfo, ':');
        // String.indexOf(':', 0, _userinfo.length, _userinfo, 0, 1, 0);
        if (to == -1) {
            return _userinfo; // only user.
        }
        char[] result = new char[to];
        System.arraycopy(_userinfo, 0, result, 0, to);
        return result;
    }


    /**
     * Get the escaped user
     *
     * @return the escaped user
     */
    public String getEscapedUser() {
        char[] user = getRawUser();
        return (user == null) ? null : new String(user);
    }


    /**
     * Get the user.
     *
     * @return the user name
     * @exception URIException
     */
    public String getUser() throws URIException {
        char[] user = getRawUser();
        return (user == null) ? null : decode(user);
    }


    /**
     * Set the raw-escaped password.
     *
     * @param password the raw-escaped password; could be null
     * @exception URIException escaped password not valid or username missed
     */
    public void setRawPassword(char[] escapedPassword) throws URIException {
        if (escapedPassword != null &&
                !validate(escapedPassword, within_userinfo))
            throw new URIException(URIException.ESCAPING,
                    "escaped password not valid");
        if (getRawUser() == null || getRawUser().length == 0)
            throw new URIException(URIException.PARSING, "username required");
        String username = new String(getRawUser());
        String password = new String(escapedPassword);
        // an emtpy string is allowed as a password
        String userinfo = username + ((password == null) ? "" : ":" + password);
        String hostname = new String(getRawHost());
        String hostport = (_port == -1) ? hostname : hostname + _port;
        String authority = userinfo + "@" + hostport;
        _userinfo = userinfo.toCharArray();
        _authority = authority.toCharArray();
        setURI();
    }


    /**
     * Set the escaped password string.
     *
     * @param password the escaped password string; could be null
     * @exception URIException escaped password not valid or username missed
     */
    public void setEscapedPassword(String escapedPassword) throws URIException {
        setRawPassword((escapedPassword == null) ? null :
                escapedPassword.toCharArray());
    }


    /**
     * Set the password string.
     *
     * @param password the password string; could be null
     * @exception URIException encoding error or username missed
     */
    public void setPassword(String password) throws URIException {
        setRawPassword((password == null) ? null :
                encode(password, allowed_within_userinfo));
    }


    /**
     * Get the raw-escaped password.
     *
     * @return the raw-escaped password
     */
    public char[] getRawPassword() {
        int from = indexFirstOf(_userinfo, ':');
        if (from == -1) {
            return null; // null or only user.
        }
        int len = _userinfo.length - from - 1;
        char[] result = new char[len];
        System.arraycopy(_userinfo, from + 1, result, 0, len);
        return result;
    }


    /**
     * Get the escaped password.
     *
     * @return the escaped password
     */
    public String getEscapedPassword() {
        char[] password = getRawPassword();
        return (password == null) ? null : new String(password);
    }


    /**
     * Get the password.
     *
     * @return the password
     * @exception URIException
     */
    public String getPassword() throws URIException {
        char[] password = getRawPassword();
        return (password == null) ? null : decode(password);
    }

    // --------------------------------------------------------------- The path

    /**
     * Get the raw-escaped current hierarchy level.
     *
     * @return the raw-escaped current hierarchy level
     * @exception URIException no hierarchy level
     */
    public char[] getRawCurrentHierPath() throws URIException {
        return (_path == null || _path.length == 0) ? rootPath :
            super.getRawCurrentHierPath(_path);
    }


    /**
     * Get the level above the this hierarchy level.
     *
     * @return the raw above hierarchy level
     * @exception URIException
     */
    public char[] getRawAboveHierPath() throws URIException {
        char[] path = getRawCurrentHierPath();
        return (path == null || path.length == 0) ? rootPath :
            getRawCurrentHierPath(path);
    }


    /**
     * Get the raw escaped path.
     *
     * @return the path '/' if empty or undefined
     */
    public char[] getRawPath() {
        char[] path =  super.getRawPath();
        return (path == null || path.length == 0) ? rootPath : path;
    }

    // -------------------------------------------------------------- The query

    /**
     * Set the query as the name and value pair.
     *
     * @param queryName the query string.
     * @param queryValue the query string.
     * @exception URIException incomplete trailing escape pattern
     * Or unsupported character encoding
     * @throws NullPointerException null query
     * @see #encode
     */
    public void setQuery(String queryName, String queryValue)
        throws URIException {

        StringBuffer buff = new StringBuffer();
        buff.append(encode(queryName, allowed_within_query));
        buff.append('=');
        buff.append(encode(queryValue, allowed_within_query));
        _query = buff.toString().toCharArray();
        setURI();
    }


    /**
     * Set the query as the name and value pairs.
     *
     * @param queryName the array of the query string.
     * @param queryValue the array of the query string.
     * @exception URIException incomplete trailing escape pattern,
     * unsupported character encoding or wrong array size
     * @throws NullPointerException null query
     * @see #encode
     */
    public void setQuery(String[] queryName, String[] queryValue)
        throws URIException {

        int length = queryName.length;
        if (length != queryValue.length)
            throw new URIException("wrong array size of query");

        StringBuffer buff = new StringBuffer();
        for (int i = 0; i < length; i++) {
            buff.append(encode(queryName[i], allowed_within_query));
            buff.append('=');
            buff.append(encode(queryValue[i], allowed_within_query));
            if (i + 1 < length) buff.append('&');
        }
        _query = buff.toString().toCharArray();
        setURI();
    }

    // ---------------------------------------------------------------- Utility

    /**
     * Verify the valid class use for construction.
     *
     * @exception URIException the wrong scheme use
     */
    protected void checkValid() throws URIException {
        // could be explicit protocol or undefined.
        if (!(equals(_scheme, _default_scheme) || _scheme == null)) {
            throw new URIException(URIException.PARSING, "wrong class use");
        }
    }

}

