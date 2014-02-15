/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/TransparentProxyRequestHandler.java,v 1.1.2.1 2003/12/05 21:02:52 oglueck Exp $
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
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.Socket;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpConstants;
import org.apache.commons.httpclient.HttpURL;
import org.apache.commons.httpclient.URI;

/**
 * This request handler can handle the CONNECT method. It does
 * nothing for any other HTTP methods.
 * 
 * @author Ortwin Glueck
 */
public class TransparentProxyRequestHandler implements HttpRequestHandler {

	/* (non-Javadoc)
	 * @see org.apache.commons.httpclient.server.HttpRequestHandler#processRequest(org.apache.commons.httpclient.server.SimpleHttpServerConnection)
	 */
	public boolean processRequest(SimpleHttpServerConnection conn) throws IOException {
		RequestLine line = conn.getRequestLine();
		String method = line.getMethod();
		if (!"CONNECT".equalsIgnoreCase(method)) return false;
		URI url = new HttpURL(line.getUri());
		handshake(conn, url);
		return true;
	}

	private void handshake(SimpleHttpServerConnection conn, URI url) throws IOException  {
		Socket targetSocket = new Socket(url.getHost(), url.getPort());
		InputStream sourceIn = conn.getInputStream();
		OutputStream sourceOut = conn.getOutputStream();
		InputStream targetIn = targetSocket.getInputStream();
		OutputStream targetOut = targetSocket.getOutputStream();

		ResponseWriter out = conn.getWriter();
		out.println("HTTP/1.1 200 Connection established");
		out.flush();

		BidiStreamProxy bdsp = new BidiStreamProxy(sourceIn, sourceOut, targetIn, targetOut);
		bdsp.start();
		try {
			bdsp.block();
		} catch (InterruptedException e) {
			throw new IOException(e.toString());
		}
	}
	
	private void sendHeaders(Header[] headers, OutputStream os) throws IOException {
		Writer out;
		try {
			out = new OutputStreamWriter(os, HttpConstants.HTTP_ELEMENT_CHARSET);
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e.toString());
		}
		for (int i=0; i<headers.length; i++) {
			out.write(headers[i].toExternalForm());
		}
	}
}