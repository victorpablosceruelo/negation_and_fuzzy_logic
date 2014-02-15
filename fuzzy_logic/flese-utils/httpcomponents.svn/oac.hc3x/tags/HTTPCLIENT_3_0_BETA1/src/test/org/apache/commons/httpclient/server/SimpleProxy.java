/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/SimpleProxy.java,v 1.6 2004/11/13 12:21:28 olegk Exp $
 * $Revision: 1.6 $
 * $Date: 2004-11-13 13:21:28 +0100 (Sat, 13 Nov 2004) $
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
 * @author Oleg Kalnichevski
 */
public class SimpleProxy extends SimpleHttpServer {
    
    private SimpleConnManager connmanager = null; 
    private HttpRequestHandlerChain chain = null;

    /**
     * @throws IOException
     */
    public SimpleProxy() throws IOException {
        super();
        this.connmanager = new SimpleConnManager(); 
        this.chain = new HttpRequestHandlerChain();
        this.chain.appendHandler(new TransparentProxyRequestHandler());
        this.chain.appendHandler(new ProxyRequestHandler(this.connmanager));
        setRequestHandler(this.chain);
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
        this.chain.prependHandler(new ProxyAuthRequestHandler(creds));
    }

    public void destroy() {
        this.connmanager.shutdown();
        super.destroy();
    }
    
}
