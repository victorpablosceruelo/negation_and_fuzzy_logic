/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/ConnectMethod.java,v 1.5 2003/01/23 22:47:45 jsdever Exp $
 * $Revision: 1.5 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
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

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * <p>Wraps another method to tunnel through a proxy.</p>
 *
 * @author Ortwin Glück
 * @author dIon Gillard
 * @since 2.0
 * @version $Revision: 1.5 $ $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
 */
public class ConnectMethod extends HttpMethodBase {
    /** the name of this method */
    public static final String NAME = "CONNECT";

    /**
     * Create a connect method wrapping the existing method
     *
     * @param method the {@link HttpMethod method} to execute after connecting
     *      to the server
     */
    public ConnectMethod(HttpMethod method) {
        log.trace("enter ConnectMethod(HttpMethod)");
        this.method = method;
    }

    /**
     * Provide the {@link #NAME name} of this method.
     *
     * @return the String "CONNECT"
     */
    public String getName() {
        return NAME;
    }

    /**
     * Execute this method by tunnelling and then executing the wrapped method.
     *
     * @param state the current http state
     * @param conn the connection to write to
     * @return the http status code from execution
     * @throws HttpException when an error occurs writing the headers
     * @throws IOException when an error occurs writing the headers
     */
    public int execute(HttpState state, HttpConnection conn) 
    throws IOException, HttpException {

        log.trace("enter ConnectMethod.execute(HttpState, HttpConnection)");
        int code = super.execute(state, conn);
        log.debug("CONNECT status code " + code);
        if ((code >= 200) && (code < 300)) {
            conn.tunnelCreated();
            code = method.execute(state, conn);
        }
        return code;
    }

    /**
     * Writes a minimal set of headers to the proxy.
     *
     * @param state the current http state
     * @param conn the connection to write to
     * @throws HttpException when an error occurs writing the headers
     * @throws IOException when an error occurs writing the headers
     */
    protected void writeRequestHeaders(HttpState state, HttpConnection conn) 
    throws HttpException, IOException {
        log.trace("enter ConnectMethod.writeRequestHeaders(HttpState, "
            + "HttpConnection)");

        if (method instanceof HttpMethodBase) {
            ((HttpMethodBase) method).addRequestHeaders(state, conn);
        }
        conn.print(method.getRequestHeader("Host").toExternalForm());
        Header header = method.getRequestHeader(Authenticator.PROXY_AUTH_RESP);
        if (header == null) {
            header = getRequestHeader(Authenticator.PROXY_AUTH_RESP);
        }
        if (header != null) {
            conn.print(header.toExternalForm());
        }
    }

    /**
     * Special Connect request.
     *
     * @param state the current http state
     * @param conn the connection to write to
     * @throws IOException when an error occurs writing the request
     * @throws HttpException when an error occurs writing the request
     */
    protected void writeRequestLine(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        int port = conn.getPort();
        if (port == -1) {
            port = conn.isSecure() ? 443 : 80;
        }
        String line = getName() + " " + conn.getHost() + ":" + port 
            + " HTTP/1.1";
        conn.printLine(line);
    }


    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(ConnectMethod.class);

    /** The wrapped method */
    private HttpMethod method;
}
