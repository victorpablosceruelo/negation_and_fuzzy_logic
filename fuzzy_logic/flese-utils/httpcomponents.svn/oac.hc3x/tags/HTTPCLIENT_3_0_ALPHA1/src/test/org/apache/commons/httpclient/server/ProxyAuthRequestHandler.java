/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/ProxyAuthRequestHandler.java,v 1.9 2004/02/27 19:01:34 olegk Exp $
 * $Revision: 1.9 $
 * $Date: 2004-02-27 20:01:34 +0100 (Fri, 27 Feb 2004) $
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

package org.apache.commons.httpclient.server;

import java.io.IOException;

import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.BasicScheme;

/**
 * This request handler guards access to a proxy when used in a request handler
 * chain. It checks the headers for valid credentials and performs the
 * authentication handshake if necessary.
 * 
 * @author Ortwin Glueck
 * @author Oleg Kalnichevski
 */
public class ProxyAuthRequestHandler implements HttpRequestHandler {
    private Credentials credentials;

    /**
     * The proxy authenticate challange header.
     */
    public static final String PROXY_AUTH = "Proxy-Authenticate";

    /**
     * The proxy authenticate response header.
     */
    public static final String PROXY_AUTH_RESP = "Proxy-Authorization";

    /**
     * TODO replace creds parameter with a class specific to an auth scheme
     * encapsulating all required information for a specific scheme
     * 
     * @param creds
     */
    public ProxyAuthRequestHandler(Credentials creds) {
        if (creds == null)
            throw new IllegalArgumentException("Credentials can not be null");
        this.credentials = creds;
    }

    public boolean processRequest(
        final SimpleHttpServerConnection conn,
        final SimpleRequest request) throws IOException
    {
        Header clientAuth = request.getFirstHeader(PROXY_AUTH_RESP);
        if (clientAuth != null) {
            boolean ok = checkAuthorization(clientAuth);
            if (ok)
                conn.connectionKeepAlive();
            return !ok;
        } else {
            performHandshake(conn);
        }
        return true;
    }

    /**
     * @param conn
     */
    private void performHandshake(SimpleHttpServerConnection conn) throws IOException {
        Header challenge = createChallenge();
        ResponseWriter out = conn.getWriter();
        out.println("HTTP/1.1 407 Proxy Authentication Required");
        out.print(challenge.toExternalForm());
        out.print(new Header("Proxy-Connection", "Keep-Alive").toExternalForm());
        out.print(new Header("Content-Length", "0").toExternalForm());
        out.println();
        out.flush();
        conn.connectionKeepAlive();
    }

    /**
     * @return
     */
    private Header createChallenge() {
        Header header = new Header();
        header.setName(PROXY_AUTH);
        //TODO add more auth schemes
        String challenge = "basic realm=test";
        header.setValue(challenge);
        return header;
    }

    /**
     * Checks if the credentials provided by the client match the required
     * credentials
     * 
     * @return true if the client is authorized, false if not.
     * @param clientAuth
     */
    private boolean checkAuthorization(Header clientAuth) {
        String expectedAuthString = BasicScheme.authenticate(
            (UsernamePasswordCredentials)credentials,
            "ISO-8859-1");
        return expectedAuthString.equals(clientAuth.getValue());
    }

}
