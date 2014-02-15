/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/ProxyRequestHandler.java,v 1.5 2004/05/12 20:43:54 olegk Exp $
 * $Revision: 1.5 $
 * $Date: 2004-05-12 22:43:54 +0200 (Wed, 12 May 2004) $
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
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
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
	public boolean processRequest(
        final SimpleHttpServerConnection conn,
        final SimpleRequest request) throws IOException
    {
		RequestLine line = request.getRequestLine();
		String method = line.getMethod();
		//TODO add POST method handling
		if (!"GET".equalsIgnoreCase(method)) return false;
		httpProxy(conn, request);
		return true;
	}

	/**
	 * @param conn
	 */
	private void httpProxy(
        final SimpleHttpServerConnection conn,
        final SimpleRequest request) throws IOException
    {
		Log wireLog = LogFactory.getLog("httpclient.wire");
		
        URI url = new HttpURL(request.getRequestLine().getUri());

		HttpClient client = new HttpClient();
		HostConfiguration hc = new HostConfiguration();
		hc.setHost(url);
		client.setHostConfiguration(hc);
		
		//TODO support other methods
		HttpMethod method = new GetMethod(url.getPathQuery());
		Header[] headers = request.getHeaders();
		for (int i=0; i<headers.length; i++) {
			method.addRequestHeader(headers[i]);
		}
		if (method instanceof EntityEnclosingMethod) {
			EntityEnclosingMethod emethod = (EntityEnclosingMethod) method;
			emethod.setRequestEntity(
                new InputStreamRequestEntity(conn.getInputStream()));
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