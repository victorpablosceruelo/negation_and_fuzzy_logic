/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestLocalHostBase.java,v 1.6 2004/06/25 03:34:56 mbecke Exp $
 * $Revision: 1.6 $
 * $Date: 2004-06-25 05:34:56 +0200 (Fri, 25 Jun 2004) $
 * ====================================================================
 *
 *  Copyright 2002-2004 The Apache Software Foundation
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
package org.apache.commons.httpclient;

import junit.framework.TestCase;

/**
 * The base class for all tests that need to connection to the localhost
 * web server.
 * 
 * @author Michael Becke
 */
public abstract class TestLocalHostBase extends TestCase {

    private final String protocol = System.getProperty(
        "httpclient.test.localHost.protocol",
        "http"
    );
    private final String host = System.getProperty("httpclient.test.localHost","localhost");
    private final int port;
    private final String proxyHost = System.getProperty("httpclient.test.proxy.host");
    private final int proxyPort;
    
    /**
     * Constructor for TestLocalHostBase.
     * @param testName
     */
    public TestLocalHostBase(String testName) {
        super(testName);
        String portString = System.getProperty("httpclient.test.localPort","8080");
        int tempPort = 8080;
        try {
            tempPort = Integer.parseInt(portString);
        } catch(Exception e) {
            tempPort = 8080;
        }
        port = tempPort;
        String proxyPortString = System.getProperty("httpclient.test.proxy.port","3128");
        int tempProxyPort = 3128;
        try {
            tempProxyPort = Integer.parseInt(proxyPortString);
        } catch(Exception e) {
            tempProxyPort = 3128;
        }
        proxyPort = tempProxyPort;
    }

    /**
     * Gets a new HttpClient instance.  This instance has been configured
     * with all appropriate host/proxy values.
     * 
     * @return a new HttpClient instance
     */
    public HttpClient createHttpClient() {
        return createHttpClient(null);
    }

    /**
     * Gets a new HttpClient instance that uses the given connection manager. 
     * This instance has been configured with all appropriate host/proxy values.
     * 
     * @param connectionManager the connection manager to use or <code>null</code>
     * 
     * @return a new HttpClient instance
     */
    public HttpClient createHttpClient(HttpConnectionManager connectionManager) {
        
        HttpClient client = null;

        if (connectionManager == null) {
            client = new HttpClient();
        } else {
            client = new HttpClient(connectionManager);
        }

        configureHostConfiguration(client.getHostConfiguration());

        return client;
    }
    
    /**
     * Configures the host config with the correct host and proxy settings.
     * 
     * @param hostConfiguration
     */
    public void configureHostConfiguration(HostConfiguration hostConfiguration) {
        hostConfiguration.setHost(host, port, protocol);
        if (proxyHost != null) {
            hostConfiguration.setProxy(proxyHost, proxyPort);
        }
    }

    /**
     * @return String
     */
    public String getHost() {
        return host;
    }

    /**
     * @return int
     */
    public int getPort() {
        return port;
    }

    /**
     * @return String
     */
    public String getProtocol() {
        return protocol;
    }

}
