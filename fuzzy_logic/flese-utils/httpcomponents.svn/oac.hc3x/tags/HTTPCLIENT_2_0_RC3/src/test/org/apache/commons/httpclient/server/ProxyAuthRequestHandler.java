/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/ProxyAuthRequestHandler.java,v 1.1.2.1 2003/12/05 21:02:52 oglueck Exp $
 * $Revision: 1.1.2.1 $
 * $Date: 2003-12-05 22:02:52 +0100 (Fri, 05 Dec 2003) $
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

package org.apache.commons.httpclient.server;

import java.io.IOException;

import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.auth.AuthenticationException;
import org.apache.commons.httpclient.auth.BasicScheme;
import org.apache.commons.httpclient.auth.HttpAuthenticator;
import org.apache.commons.httpclient.auth.MalformedChallengeException;

/**
 * This request handler guards access to a proxy when used in a 
 * request handler chain. It checks the headers for valid credentials
 * and performs the authentication handshake if necessary.
 * 
 * @author Ortwin Glueck
 */
public class ProxyAuthRequestHandler implements HttpRequestHandler {
	private Credentials credentials;
	
	/**
	 * TODO replace creds parameter with a class specific to an auth scheme encapsulating all required information for a specific scheme
	 * @param creds
	 */
	public ProxyAuthRequestHandler(Credentials creds)  {
		if (creds == null) throw new IllegalArgumentException("Credentials can not be null");
		this.credentials = creds;
	}

	public boolean processRequest(SimpleHttpServerConnection conn)
		throws IOException {
		Header[] headers = conn.getHeaders();
		Header clientAuth = findHeader(headers, HttpAuthenticator.PROXY_AUTH_RESP);
		if (clientAuth != null) {
			boolean ok = checkAuthorization(clientAuth);
			if (ok) conn.connectionKeepAlive();
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
	 * 
	 * @return
	 */
	private Header createChallenge() {
		Header header = new Header();
		header.setName(HttpAuthenticator.PROXY_AUTH);
		//TODO add more auth schemes
		String challenge = "basic realm=test";
		header.setValue(challenge);
		return header;
	}

	/**
	 * Checks if the credentials provided by the client match the required credentials
	 * @return true if the client is authorized, false if not.
	 * @param clientAuth
	 */
	private boolean checkAuthorization(Header clientAuth) {
		// TODO Auto-generated method stub
		BasicScheme scheme;
		try {
			scheme = new BasicScheme("basic realm=test");
			String expectedAuthString = scheme.authenticate(credentials, null, null);
			return expectedAuthString.equals(clientAuth.getValue());
		} catch (MalformedChallengeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (AuthenticationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
	}

	private Header findHeader(Header[] headers, String name) {
		for(int i=0; i<headers.length; i++) {
			Header header = headers[i];
			if (header.getName().equalsIgnoreCase(name)) return header;
		}
		return null;
	}

}
