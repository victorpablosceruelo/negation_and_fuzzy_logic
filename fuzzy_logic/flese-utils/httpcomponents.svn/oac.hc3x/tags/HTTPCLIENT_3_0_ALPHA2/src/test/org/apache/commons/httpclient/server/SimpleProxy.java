/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/SimpleProxy.java,v 1.4 2004/02/22 18:08:52 olegk Exp $
 * $Revision: 1.4 $
 * $Date: 2004-02-22 19:08:52 +0100 (Sun, 22 Feb 2004) $
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

/**
 * Simple server that registers default request handlers to act as a proxy.
 * 
 * @author Ortwin Glueck
 */
public class SimpleProxy extends SimpleHttpServer {
    private HttpRequestHandlerChain chain = new SimpleChain();

    /**
     * @throws IOException
     */
    public SimpleProxy() throws IOException {
        setRequestHandler(chain);
    }

    /**
     * @param port
     *                The local TCP port to listen on
     * @throws IOException
     */
    public SimpleProxy(int port) throws IOException {
        super(port);
    }

    public void requireCredentials(Credentials creds) {
        chain.prependHandler(new ProxyAuthRequestHandler(creds));
    }

    private class SimpleChain extends HttpRequestHandlerChain {
        public SimpleChain() {
            appendHandler(new TransparentProxyRequestHandler());
            appendHandler(new ProxyRequestHandler());
        }
    }

}
