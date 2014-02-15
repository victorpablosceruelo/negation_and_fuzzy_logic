/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/Authenticator.java,v 1.39 2003/01/28 04:40:20 jsdever Exp $
 * $Revision: 1.39 $
 * $Date: 2003-01-28 05:40:23 +0100 (Tue, 28 Jan 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
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

package org.apache.commons.httpclient;

import org.apache.commons.httpclient.util.Base64;

import java.security.MessageDigest;
import java.util.Map;
import java.util.Hashtable;
import java.util.StringTokenizer;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Utility methods for HTTP authorization and authentication.  This class
 * provides utility methods for generating responses to HTTP www and proxy
 * authentication challenges.
 * 
 * <p>
 * Preemptive authentication can be turned on by using the property value of
 * #PREEMPTIVE_PROPERTY.  If left unspecified, it has the default value of
 * #PREEMPTIVE_DEFAULT.  This configurable behaviour conforms to rcf2617:
 * <blockquote>
 * A client SHOULD assume that all paths at or deeper than the depth of the
 * last symbolic element in the path field of the Request-URI also are within
 * the protection space specified by the Basic realm value of the current
 * challenge. A client MAY preemptively send the corresponding Authorization
 * header with requests for resources in that space without receipt of another
 * challenge from the server. Similarly, when a client sends a request to a
 * proxy, it may reuse a userid and password in the Proxy-Authorization header
 * field without receiving another challenge from the proxy server.
 * </blockquote>
 * </p>
 * 
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Rodney Waldhoff
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author Ortwin Glück
 * @author Sean C. Sullivan
 * @author <a href="mailto:adrian@ephox.com">Adrian Sutton</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @version $Revision: 1.39 $ $Date: 2003-01-28 05:40:23 +0100 (Tue, 28 Jan 2003) $
 */
public class Authenticator {

    // -------------------------------------- Static variables and initializers

    /**
     * <tt>org.apache.commons.httpclient.Authenticator</tt> LOG.
     */
    private static final Log LOG = LogFactory.getLog(Authenticator.class);


    /**
     * The boolean property name to turn on preemptive authentication.
     */
    public static final String PREEMPTIVE_PROPERTY = 
        "httpclient.authentication.preemptive";


    /**
     * The default property value for #PREEMPTIVE_PROPERTY.
     */
    public static final String PREEMPTIVE_DEFAULT = "false";


    /**
     * The www authenticate challange header.
     */
    public static final String WWW_AUTH = "WWW-Authenticate";


    /**
     * The www authenticate response header.
     */
    public static final String WWW_AUTH_RESP = "Authorization";


    /**
     * The proxy authenticate challange header.
     */
    public static final String PROXY_AUTH = "Proxy-Authenticate";


    /**
     * The proxy authenticate response header.
     */
    public static final String PROXY_AUTH_RESP = "Proxy-Authorization";


    /**
     * Hexa values used when creating 32 character long digest in HTTP Digest
     * in case of authentication.
     * 
     * @see #encode(byte[])
     */
    private static final char[] HEXADECIMAL = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 
        'e', 'f'
    };

    // ---------------------------------------------------------------- Methods

    /**
     * Creates an MD5 response digest.
     * 
     * @param uname Username
     * @param pwd Password
     * @param mapCredentials map containing necessary header parameters to
     *        construct the digest. It must/can contain: uri, realm, nonce,
     *        cnonce, qop, nc.
     * @return The created digest as string. This will be the response tag's
     *         value in the Authentication HTTP header.
     * @throws HttpException when MD5 is an unsupported algorithm
     * TODO: + Add createDigest() method 
     */
    public static String createDigest(String uname, String pwd,
            Map mapCredentials) throws HttpException {

        LOG.trace("enter Authenticator.createDigest(String, String, Map)");

        final String digAlg = "MD5";

        // Collecting required tokens
        String uri = removeQuotes((String) mapCredentials.get("uri"));
        String realm = removeQuotes((String) mapCredentials.get("realm"));
        String nonce = removeQuotes((String) mapCredentials.get("nonce"));
        String nc = removeQuotes((String) mapCredentials.get("nc"));
        String cnonce = removeQuotes((String) mapCredentials.get("cnonce"));
        String qop = removeQuotes((String) mapCredentials.get("qop"));
        String method = (String) mapCredentials.get("methodname");

        if (qop != null) {
            qop = "auth";
        }

        MessageDigest md5Helper;

        try {
            md5Helper = MessageDigest.getInstance(digAlg);
        } catch (Exception e) {
            LOG.error("ERROR! Unsupported algorithm in HTTP Digest "
                      + "authentication: " + digAlg, e);
            throw new HttpException("Unsupported algorithm in HTTP Digest "
                                    + "authentication: " + digAlg);
        }

        // Calculating digest according to rfc 2617
        String a2 = method + ":" + uri;
        String md5a2 = encode(md5Helper.digest(HttpConstants.getBytes(a2)));
        String digestValue = uname + ":" + realm + ":" + pwd;
        String md5a1 
            = encode(md5Helper.digest(HttpConstants.getBytes(digestValue)));
        String serverDigestValue;

        if (qop == null) {
            serverDigestValue = md5a1 + ":" + nonce + ":" + md5a2;
        } else {
            serverDigestValue = md5a1 + ":" + nonce + ":" + nc + ":" + cnonce
                                + ":" + qop + ":" + md5a2;
        }

        String serverDigest =
            encode(md5Helper.digest(HttpConstants.getBytes(serverDigestValue)));

        return serverDigest;
    }


    /**
     * Add requisite authentication credentials to the given <i>method</i> in
     * the given <i>state</i> if possible.
     * 
     * @param method the HttpMethod which requires authentication
     * @param state the HttpState object providing Credentials
     * @return true if the Authenticate response header was added
     * @throws HttpException when a parsing or other error occurs
     * @throws UnsupportedOperationException when the challenge type is not
     *         supported
     * @see HttpState#setCredentials(String,Credentials)
     */
    public static boolean authenticate(HttpMethod method, HttpState state)
        throws HttpException, UnsupportedOperationException {

        LOG.trace("enter Authenticator.authenticate(HttpMethod, HttpState)");

        Header challengeHeader = method.getResponseHeader(WWW_AUTH);

        return authenticate(method, state, challengeHeader, WWW_AUTH_RESP);
    }


    /**
     * Add requisite proxy authentication credentials to the given
     * <i>method</i> in the given <i>state</i> if possible.
     * 
     * @param method the HttpMethod which requires authentication
     * @param state the HttpState object providing Credentials
     * @return true if the Authenticate response header was added
     * @throws HttpException when a parsing or other error occurs
     * @throws UnsupportedOperationException when the given challenge type is
     *         not supported
     * @see HttpState#setProxyCredentials(String,Credentials)
     */
    public static boolean authenticateProxy(HttpMethod method, HttpState state)
        throws HttpException, UnsupportedOperationException {

        LOG.trace("enter Authenticator.authenticateProxy(HttpMethod, "
                  + "HttpState)");

        Header challengeHeader = method.getResponseHeader(PROXY_AUTH);

        return authenticate(method, state, challengeHeader, PROXY_AUTH_RESP);
    }


    /**
     * Return a Basic <tt>Authorization</tt> header value for the given {@link
     * UsernamePasswordCredentials}.
     * 
     * @param credentials the credentials to encode. Must be non-null
     * @return the credentials as a Basic Authentication string
     */
    private static String basic(UsernamePasswordCredentials credentials) {
        LOG.trace("enter Authenticator.basic(UsernamePasswordCredentials)");

        final String authString = credentials.getUserName() + ":"
            + credentials.getPassword();

        return "Basic " + HttpConstants.getString(
            Base64.encode(HttpConstants.getBytes(authString)));
    }


    /**
     * Create a Basic <tt>Authorization</tt> header for the given <i>realm</i>
     * and <i>state</i> to the given <i>method</i>.
     * 
     * @param realm the basic realm to authenticate to
     * @param state a {@link HttpState} object providing {@link Credentials}
     * @param responseHeader the header's name to store the authentication
     *        response in. PROXY_AUTH_RESP will force the proxy credentials to
     *        be used.
     * @return a basic <tt>Authorization</tt> header
     * @throws HttpException when no matching credentials are available
     */
    private static Header basic(String realm, HttpState state,
            String responseHeader) throws HttpException {

        LOG.trace("enter Authenticator.basic(String, HttpState, String)");

        boolean proxy = PROXY_AUTH_RESP.equals(responseHeader);
        UsernamePasswordCredentials credentials = null;

        try {
            credentials = (UsernamePasswordCredentials) (proxy
                ? state.getProxyCredentials(realm)
                : state.getCredentials(realm));
        } catch (ClassCastException e) {
            throw new HttpException("UsernamePasswordCredentials required for "
                                    + "Basic authentication.");
        }

        if (credentials == null) {
            throw new HttpException("No credentials available for the Basic "
                                    + "authentication realm \'" + realm + "\'");
        }
        // else
        return new Header(responseHeader, Authenticator.basic(credentials));
    }


    /**
     * Create a NTLM <tt>Authorization</tt> header for the given
     * <i>Challenge</i> and <i>state</i> to the given <i>method</i>.
     *
     * @param challenge The challenge.
     * @param method the {@link HttpMethod request} requiring the ntlm
     * @param state a {@link HttpState} object providing
     * {@link Credentials}
     * @param responseHeader the header's name to store the authentication
     * response in.  PROXY_AUTH_RESP will force the proxy credentials to
     * be used.
     * @return a ntlm <tt>Authorization</tt> header
     * @throws HttpException when no matching credentials are available
     */
    private static Header ntlm(String challenge, HttpMethod method,
            HttpState state, String responseHeader) throws HttpException {

        LOG.trace("enter Authenticator.ntlm(String, HttpMethod, HttpState, "
                  + "String)");

        boolean proxy = PROXY_AUTH_RESP.equals(responseHeader);

        NTCredentials credentials = null;

        if (method.getRequestHeader("Host") != null) {
            String host = method.getRequestHeader("Host").getValue();
            try {
                credentials = (NTCredentials) (proxy
                    ? state.getProxyCredentials(host)
                    : state.getCredentials(host));
            } catch (ClassCastException e) {
                throw new HttpException("NTCredentials required "
                                        + "for NTLM authentication.");
            }
        }

        if (credentials == null) {
            LOG.info("No credentials for specific host, " 
                + "attempting to use default credentials.");
            try {
                credentials = (NTCredentials) (proxy
                    ? state.getProxyCredentials(null)
                    : state.getCredentials(null));
            } catch (ClassCastException e) {
                throw new HttpException(
                        "NTCredentials required for NTLM authentication.");
            }
        }

        try {
            challenge =
                challenge.substring(challenge.toLowerCase().indexOf("ntlm")
                    + "ntlm".length()).trim();
        } catch (IndexOutOfBoundsException e) {
            throw new HttpException("Invalid NTLM challenge.");
        }

        if (credentials == null) {
            throw new HttpException("No credentials available for NTLM "
                + "authentication.");
        } else {
            NTLM ntlm = new NTLM();
            String response = "NTLM " + ntlm.getResponseFor(challenge,
                    credentials.getUserName(), credentials.getPassword(),
                    credentials.getHost(), credentials.getDomain());
            if (LOG.isDebugEnabled()) {
                LOG.debug("Replying to challenge with: " + response);
            }
            return new Header(responseHeader, response);
        }
    }


    /**
     * Create a Digest <tt>Authorization</tt> header for the given <i>realm</i>
     * and <i>state</i> to the given <i>method</i>.
     * 
     * @param realm the basic realm to authenticate to
     * @param method the {@link HttpMethod request} requiring the digest
     * @param state a {@link HttpState} object providing {@link Credentials}
     * @param responseHeader the header's name to store the authentication
     *        response in. PROXY_AUTH_RESP will force the proxy credentials to
     *        be used.
     * @return a digest <tt>Authorization</tt> header
     * @throws HttpException when no matching credentials are available
     */
    private static Header digest(String realm, HttpMethod method,
            HttpState state, String responseHeader) throws HttpException {

        LOG.trace("enter Authenticator.digest(String, HttpMethod, HttpState, "
                  + "String)");

        boolean proxy = PROXY_AUTH_RESP.equals(responseHeader);
        UsernamePasswordCredentials credentials = null;

        try {
            credentials = (UsernamePasswordCredentials) (proxy
                ? state.getProxyCredentials(realm)
                : state.getCredentials(realm));
        } catch (ClassCastException e) {
            throw new HttpException("UsernamePasswordCredentials required for "
                                    + "Digest authentication.");
        }

        if (credentials == null) {
            if (LOG.isInfoEnabled()) {
                LOG.info("No credentials found for realm \"" + realm + "\", "
                         + "attempting to use default credentials.");
            }

            try {
                credentials = (UsernamePasswordCredentials) (proxy
                    ? state.getProxyCredentials(null)
                    : state.getCredentials(null));
            } catch (ClassCastException e) {
                throw new HttpException("UsernamePasswordCredentials required "
                                        + "for Digest authentication.");
            }
        }

        if (credentials == null) {
            throw new HttpException("No credentials available for the Digest "
                + "authentication realm \"" + realm + "\"/");
        } else {
            Map headers = getHTTPDigestCredentials(method, proxy);
            headers.put("cnonce", "\"" + createCnonce() + "\"");
            headers.put("nc", "00000001");
            headers.put("uri", method.getPath());
            headers.put("methodname", method.getName());

            return new Header(responseHeader, Authenticator.digest(credentials,
                        headers));
        }
    }


    /**
     * Return a Digest <tt>Authorization</tt> header value for the given {@link
     * UsernamePasswordCredentials}.
     *
     * @param credentials Credentials to create the digest with
     * @param mapHeaders The headers for the current request
     * @return a string containing the authorization header for digest
     * @throws HttpException When a recoverable error occurs
     */
    private static String digest(UsernamePasswordCredentials credentials,
            Map mapHeaders) throws HttpException {

        LOG.trace("enter Authenticator.digest(UsernamePasswordCredentials, "
                  + "Map)");

        String digest = createDigest(credentials.getUserName(),
                credentials.getPassword(), mapHeaders);

        return "Digest " + createDigestHeader(credentials.getUserName(),
                mapHeaders, digest);
    }


    /**
     * Processes the authenticate HTTP header received from the server that
     * requires Digest authentication.
     * 
     * @param method The HTTP method.
     * @param proxy true if authorizing for a proxy
     * 
     * @return The parameters from the authenticate header as a Map or
     *         an empty Map if there is no valid authorization.
     * 
     * @since 2.0
     * @see #processDigestToken(String, java.util.Map)
     * @see PROXY_AUTH
     * @see WWW_AUTH
     * 
     */
    private static Map getHTTPDigestCredentials(HttpMethod method, 
                                                      boolean proxy) {
        LOG.trace("enter Authenticator.getHTTPDigestCredentials(HttpMethod, "
            + "boolean)");

        //Determine wether to use proxy or www header
        String authName = proxy ? PROXY_AUTH : WWW_AUTH;
        String authHeader = null;

        //Get the authorization header value
        try {
            authHeader = method.getResponseHeader(authName).getValue();
            authHeader = authHeader.substring(7).trim();
        } catch (NullPointerException npe) {
            return (Map) (new java.util.Hashtable(0));
        }

        // map of digest tokens
        Map mapTokens = new Hashtable(17);

        //parse the authenticate header
        int i = 0;
        int j = authHeader.indexOf(",");

        while (j >= 0) {
            processDigestToken(authHeader.substring(i, j), mapTokens);
            i = j + 1;
            j = authHeader.indexOf(",", i);
        }

        if (i < authHeader.length()) {
            processDigestToken(authHeader.substring(i), mapTokens);
        }

        return mapTokens;
    }

    /** 
     * Parses an authenticate header into a map of authentication challenges
     * keyed on the lowercase authentication scheme.
     *
     * @param authHeader the authentication header
     * @return a map of authentication challenges or an empty map if the
     * <i>authHeader</i> is <tt>null</tt> or contains a <tt>null</tt> value
     * @since 2.0
     */
    private static Map parseAuthenticateHeader(Header authHeader) {

        LOG.trace("enter parseAuthenticateHeader(Header)");
        
        if (LOG.isDebugEnabled()) {
            LOG.debug("Attempting to parse authenticate header: '"
                      + authHeader + "'");
        }
        if (authHeader == null || authHeader.getValue() == null) {
            return new Hashtable(0);
        }

        String authValue = authHeader.getValue().trim();
        Map challengeMap = new Hashtable(7);

        final int authValueLength = authValue.length();
        int atStart = authValueLength > 0 ? 0 : -1; // start position
        int atQuote1 = 0; // position of quote 1
        int atQuote2 = 0; // position of quote 2
        int atComma; // position of comma
        int atSpace; // position of blank

        String challenge = null; // an authentication challenge
        String scheme = null; // the scheme from the challenge

        try {
            while (atStart >= 0 && atStart < authValueLength) {
                atQuote1 = authValue.indexOf('"', atStart);
                atQuote2 = authValue.indexOf('"', atQuote1 + 1);
                atComma = authValue.indexOf(',', atStart);

                // skip any commas in quotes
                while (atComma > atQuote1 && atComma < atQuote2 
                    && atComma > 0) {
                    atComma = authValue.indexOf(',', atComma + 1);
                }
                
                // set atComma to be the end if there is no comma
                if (atComma < 0) {
                    atComma = authValueLength;
                }

                if (LOG.isDebugEnabled()) {
                    LOG.debug("atStart =" + atStart + ", atQuote1 ="
                        + atQuote1 + ", atQuote2=" + atQuote2 + ", atComma ="
                        + atComma);
                }

                try {
                    //pull the current challenge and advance the start
                    challenge = authValue.substring(atStart, atComma).trim();
                    atStart = atComma + 1;

                    //find the blank and parse out the scheme
                    atSpace = challenge.indexOf(' ');
                    scheme = (atSpace > 0) 
                        ? challenge.substring(0, atSpace).trim() : challenge;

                    //store the challenge keyed on the scheme
                    challengeMap.put(scheme.toLowerCase(), challenge);
                    if (LOG.isDebugEnabled()) {
                        LOG.debug(scheme.toLowerCase() + "=>" + challenge);
                    }

                } catch (StringIndexOutOfBoundsException e) {
                    LOG.warn("Parsing authorization challenge'" + challenge 
                        + "' failed", e);
                }
            } // end of while
            
        } catch (StringIndexOutOfBoundsException e) {
            LOG.warn("Parsing authorization header value'" + authValue 
                + "' failed", e);
        }

        return challengeMap;
    }


    /**
     * Add requisite authentication credentials to the given <i>method</i>
     * using the given the <i>challengeHeader</i>. Currently <b>Basic</b> and
     * <b>Digest</b> authentication are supported. If the challengeHeader is
     * null, the default authentication credentials will be sent.
     * 
     * @param method the http method to add the authentication header to
     * @param authenticateHeader the header the web server created to challenge
     *        the credentials
     * @param state the http state object providing {@link Credentials}
     * @param responseHeader the response header to add (e.g. proxy or standard)
     * @return true if a response header was added
     * @throws HttpException when an error occurs parsing the challenge
     * @throws UnsupportedOperationException when the given challenge type is
     *         not supported
     * @see #basic
     * @see #digest
     * @see HttpMethod#addRequestHeader
     */
    private static boolean authenticate(HttpMethod method, HttpState state, 
            Header authenticateHeader, String responseHeader)
        throws HttpException, UnsupportedOperationException {

        LOG.trace("enter Authenticator.authenticate(HttpMethod, HttpState, "
                  + "Header, String)");

        // check the preemptive policy
        // TODO: this needs to be a service from some configuration class
        String preemptiveDefault =
            System.getProperties().getProperty(PREEMPTIVE_PROPERTY,
                    PREEMPTIVE_DEFAULT);
        preemptiveDefault = preemptiveDefault.trim().toLowerCase();

        if (!(preemptiveDefault.equals("true")
                    || preemptiveDefault.equals("false"))) { // property problem
            LOG.warn("Configuration property " + PREEMPTIVE_PROPERTY
                     + " must be either true or false.  Using default: "
                     + PREEMPTIVE_DEFAULT);
            preemptiveDefault = PREEMPTIVE_DEFAULT;
        }

        boolean preemptive = ("true".equals(preemptiveDefault));

        //if there is no challenge, attempt to use preemptive authorization
        if (authenticateHeader == null) {
            if (preemptive) {
                LOG.debug("Preemptively sending default basic credentials");

                try {
                    Header requestHeader = Authenticator.basic(null, state, 
                                                               responseHeader);
                    method.addRequestHeader(requestHeader);
                    return true;
                } catch (HttpException httpe) {
                    if (LOG.isDebugEnabled()) {
                        LOG.debug(
                                "No default credentials to preemptively send");
                    }
                    return false;
                }
            }
            // else { // no challenge and no default credentials so do nothing
            return false;
        }

        // parse the authenticate header
        Map challengeMap = parseAuthenticateHeader(authenticateHeader);

        // determine the most secure request header to add
        Header requestHeader = null;
        if (challengeMap.containsKey("ntlm")) {
            String challenge = (String) challengeMap.get("ntlm");
            requestHeader = Authenticator.ntlm(challenge, method, state,
                    responseHeader);
        } else if (challengeMap.containsKey("digest")) {
            String challenge = (String) challengeMap.get("digest");
            String realm = parseRealmFromChallenge(challenge);
            requestHeader = Authenticator.digest(realm, method, state,
                    responseHeader);
        } else if (challengeMap.containsKey("basic")) {
            String challenge = (String) challengeMap.get("basic");
            String realm = parseRealmFromChallenge(challenge);
            requestHeader = Authenticator.basic(realm, state, responseHeader);
        } else if (challengeMap.size() == 0) {
            throw new HttpException("No authentication scheme found in '"
                    + authenticateHeader + "'");
        } else {
            throw new UnsupportedOperationException(
                    "Requested authentication scheme " + challengeMap.keySet()
                    + " is unsupported");
        }

        // Add the header if it has been created and return true 
        if (requestHeader != null) { // add the header
            method.addRequestHeader(requestHeader);
            return true;
        }
        // else { // don't add the header
        return false;
    }


    /**
     * Creates a random cnonce value based on the current time.
     * 
     * @return The cnonce value as String.
     * @throws HttpException if MD5 algorithm is not supported.
     * TODO: + Add createCnonce() method
     */
    private static String createCnonce() throws HttpException {
        LOG.trace("enter Authenticator.createCnonce()");

        String cnonce;
        final String digAlg = "MD5";
        MessageDigest md5Helper;

        try {
            md5Helper = MessageDigest.getInstance(digAlg);
        } catch (Exception e) {
            LOG.error("ERROR! Unsupported algorithm in HTTP Digest "
                      + "authentication: " + digAlg);
            throw new HttpException("Unsupported algorithm in HTTP Digest "
                                    + "authentication: " + digAlg);
        }

        cnonce = Long.toString(System.currentTimeMillis());
        cnonce = encode(md5Helper.digest(HttpConstants.getBytes(cnonce)));

        return cnonce;
    }


    /**
     * Creates the header information that must be specified after the "Digest"
     * string in the HTTP Authorization header (digest-response in RFC2617).
     * 
     * @param uname Username
     * @param mapCredentials Map containing header information (uri, realm,
     *        nonce, nc, cnonce, opaque, qop).
     * @param digest The response tag's value as String.
     * @return The digest-response as String.
     * TODO: + Add createDigestHeader() method 
     */
    private static String createDigestHeader(String uname, Map mapCredentials,
            String digest) {

        LOG.trace("enter Authenticator.createDigestHeader(String, Map, "
            + "String)");

        StringBuffer sb = new StringBuffer();
        String uri = removeQuotes((String) mapCredentials.get("uri"));
        String realm = removeQuotes((String) mapCredentials.get("realm"));
        String nonce = removeQuotes((String) mapCredentials.get("nonce"));
        String nc = removeQuotes((String) mapCredentials.get("nc"));
        String cnonce = removeQuotes((String) mapCredentials.get("cnonce"));
        String opaque = removeQuotes((String) mapCredentials.get("opaque"));
        String response = digest;
        String qop = removeQuotes((String) mapCredentials.get("qop"));

        if (qop != null) {
            qop = "auth"; //we only support auth
        }

        String algorithm = "MD5"; //we only support MD5

        sb.append("username=\"" + uname + "\"")
          .append(", realm=\"" + realm + "\"")
          .append(", nonce=\"" + nonce + "\"").append(", uri=\"" + uri + "\"")
          .append(((qop == null) ? "" : ", qop=\"" + qop + "\""))
          .append(", algorithm=\"" + algorithm + "\"")
          .append(((qop == null) ? "" : ", nc=" + nc))
          .append(((qop == null) ? "" : ", cnonce=\"" + cnonce + "\""))
          .append(", response=\"" + response + "\"")
          .append((opaque == null) ? "" : ", opaque=\"" + opaque + "\"");

        return sb.toString();
    }


    /**
     * Encodes the 128 bit (16 bytes) MD5 digest into a 32 characters long 
     * <CODE>String</CODE> according to RFC 2617.
     * 
     * @param binaryData array containing the digest
     * @return encoded MD5, or <CODE>null</CODE> if encoding failed
     * TODO: + Add encode() method 
     */
    private static String encode(byte[] binaryData) {
        LOG.trace("enter Authenticator.encode(byte[])");

        if (binaryData.length != 16) {
            return null;
        } 

        char[] buffer = new char[32];
        for (int i = 0; i < 16; i++) {
            int low = (int) (binaryData[i] & 0x0f);
            int high = (int) ((binaryData[i] & 0xf0) >> 4);
            buffer[i * 2] = HEXADECIMAL[high];
            buffer[(i * 2) + 1] = HEXADECIMAL[low];
        }

        return new String(buffer);
    }


    /**
     * Parse the realm from the authentication challenge
     * 
     * @param challenge the authentication challenge
     * @return the realm
     * @throws HttpException when the realm can't be parsed
     */
    private static String parseRealmFromChallenge(String challenge)
        throws HttpException {

        // FIXME: Note that this won't work if there is more than one realm
        // within the challenge
        try {
            StringTokenizer strtok = new StringTokenizer(challenge, "=");
            String realmName = strtok.nextToken().trim();
            String realm = strtok.nextToken().trim();
            int atFirst = realm.indexOf('"');
            int atLast = realm.lastIndexOf('"');

            if ((atFirst + 1) < atLast) {
                realm = realm.substring(atFirst + 1, atLast);
            }

            if (LOG.isDebugEnabled()) {
                LOG.debug("Parsed realm '" + realm + "' from challenge '"
                          + challenge + "'");
            }

            return realm;
        } catch (Exception ex) {
            throw new HttpException("Failed to parse realm from challenge '"
                                    + challenge + "'");
        }
    }


    /**
     * Takes an entry of <CODE>"xxx=yyy"</CODE> format, partitions into a key
     * and a value (key will be the left side, value will be the right side of
     * the equal sign) and places that into a <CODE>Map</CODE>.
     * 
     * @param token the entry to be processed
     * @param tokens the <CODE>java.util.Map</CODE> into which the processed
     *        entry is placed (only if it has <CODE>"xxx=yyy"</CODE> format).
     * TODO: + Add processDigestToken() method 
     */
    private static void processDigestToken(String token, Map tokens) {
        LOG.trace("enter Authenticator.processDigestToken(String, Map)");

        int atEqual = token.indexOf("=");

        if ((atEqual > 0) && (atEqual < (token.length() - 1))) {
            tokens.put(token.substring(0, atEqual).trim(),
                    token.substring(atEqual + 1).trim());
        }
    }


    /**
     * Takes a <CODE>String</CODE> and cuts its prefix until the first double
     * quotation mark and its suffix from the last double quotation mark (and
     * cuts also the quotation marks).
     * 
     * @param str the <CODE>String</CODE> from which the prefix and suffix is
     *        to be cut.
     * @return the stumped <CODE>String</CODE> if the format of
     *         <CODE>str</CODE> is <CODE>""</CODE>; Otherwise the return value
     *         is same as <CODE>str</CODE>
     */
    private static String removeQuotes(String str) {
        LOG.trace("enter Authenticator.removeQuotes(String)");

        if (str == null) {
            return null;
        } 

        int atFirst = str.indexOf("\"") + 1;
        int atLast = str.lastIndexOf("\"");

        return (atFirst > 0 && atLast > atFirst) 
            ? str.substring(atFirst, atLast) : str;
    }

}

