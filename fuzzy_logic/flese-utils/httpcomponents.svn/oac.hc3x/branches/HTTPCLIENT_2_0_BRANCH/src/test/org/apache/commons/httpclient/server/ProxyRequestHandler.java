/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/ProxyRequestHandler.java,v 1.1.2.2 2004/02/22 18:21:18 olegk Exp $
 * $Revision: 1.1.2.2 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
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
import java.io.InputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpURL;
import org.apache.commons.httpclient.URI;
import org.apache.commons.httpclient.methods.EntityEnclosingMethod;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * This request handler can handle GET and POST requests. It does
 * nothing for all other request
 * @author Ortwin Glueck
 */
public class ProxyRequestHandler implements HttpRequestHandler {

	/**
	 * @see org.apache.commons.httpclient.server.HttpRequestHandler#processRequest(org.apache.commons.httpclient.server.SimpleHttpServerConnection)
	 */
	public boolean processRequest(SimpleHttpServerConnection conn) throws IOException {
		RequestLine line = conn.getRequestLine();
		String method = line.getMethod();
		//TODO add POST method handling
		if (!"GET".equalsIgnoreCase(method)) return false;
		URI url = new HttpURL(line.getUri());
		httpProxy(conn, url);
		return true;
	}

	/**
	 * @param conn
	 */
	private void httpProxy(SimpleHttpServerConnection conn, URI url) throws IOException {
		Log wireLog = LogFactory.getLog("httpclient.wire");
		
		HttpClient client = new HttpClient();
		HostConfiguration hc = new HostConfiguration();
		hc.setHost(url);
		client.setHostConfiguration(hc);
		
		//TODO support other methods
		HttpMethod method = new GetMethod(url.getPathQuery());
		Header[] headers = conn.getHeaders();
		for (int i=0; i<headers.length; i++) {
			method.addRequestHeader(headers[i]);
		}
		if (method instanceof EntityEnclosingMethod) {
			EntityEnclosingMethod emethod = (EntityEnclosingMethod) method;
			emethod.setRequestBody(conn.getInputStream());
		}
		client.executeMethod(method);

		
		Header[] rheaders = method.getResponseHeaders();
		InputStream targetIn = method.getResponseBodyAsStream();
		ResponseWriter out = conn.getWriter();
		out.println(method.getStatusLine().toString());
		for (int i=0; i<rheaders.length; i++) {
			out.print(rheaders[i].toExternalForm());
		}
		if (rheaders.length > 0) out.println();
		out.flush();
		out = null;
		StreamProxy sp = new StreamProxy(targetIn, conn.getOutputStream());
		sp.start();
		try {
			sp.block();
		} catch (InterruptedException e) {
			throw new IOException(e.toString());
		}
	}

}