/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/SimpleHttpConnectionManager.java,v 1.12.2.2 2004/02/22 18:21:13 olegk Exp $
 * $Revision: 1.12.2.2 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
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

import java.io.IOException;
import java.io.InputStream;

/**
 * A connection manager that provides access to a single HttpConnection.  This
 * manager makes no attempt to provide exclusive access to the contained
 * HttpConnection.
 *
 * @author <a href="mailto:becke@u.washington.edu">Michael Becke</a>
 * @author Eric Johnson
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * @author Laura Werner
 * 
 * @since 2.0
 */
public class SimpleHttpConnectionManager implements HttpConnectionManager {

    /** The http connection */
    private HttpConnection httpConnection;

    /** The value to set when calling setStaleCheckingEnabled() on connections */
    private boolean connectionStaleCheckingEnabled = true;

    /**
     * Constructor for SimpleHttpConnectionManager.
     */
    public SimpleHttpConnectionManager() {
        super();
    }

    /**
     * @see HttpConnectionManager#getConnection(HostConfiguration)
     */
    public HttpConnection getConnection(HostConfiguration hostConfiguration) {
        return getConnection(hostConfiguration, 0);
    }

    /**
     * Gets the staleCheckingEnabled value to be set on HttpConnections that are created.
     * 
     * @return <code>true</code> if stale checking will be enabled on HttpConections
     * 
     * @see HttpConnection#isStaleCheckingEnabled()
     */
    public boolean isConnectionStaleCheckingEnabled() {
        return connectionStaleCheckingEnabled;
    }

    /**
     * Sets the staleCheckingEnabled value to be set on HttpConnections that are created.
     * 
     * @param connectionStaleCheckingEnabled <code>true</code> if stale checking will be enabled 
     * on HttpConections
     * 
     * @see HttpConnection#setStaleCheckingEnabled(boolean)
     */
    public void setConnectionStaleCheckingEnabled(boolean connectionStaleCheckingEnabled) {
        this.connectionStaleCheckingEnabled = connectionStaleCheckingEnabled;
    }

    /**
     * @see HttpConnectionManager#getConnection(HostConfiguration, long)
     */
    public HttpConnection getConnection(
        HostConfiguration hostConfiguration, long timeout) {

        if (httpConnection == null) {
            httpConnection = new HttpConnection(hostConfiguration);
            httpConnection.setStaleCheckingEnabled(connectionStaleCheckingEnabled);
        } else {

            // make sure the host and proxy are correct for this connection
            // close it and set the values if they are not
            if (!hostConfiguration.hostEquals(httpConnection)
                || !hostConfiguration.proxyEquals(httpConnection)) {
                    
                if (httpConnection.isOpen()) {
                    httpConnection.close();
                }

                httpConnection.setStaleCheckingEnabled(connectionStaleCheckingEnabled);

                httpConnection.setHost(hostConfiguration.getHost());
                httpConnection.setVirtualHost(hostConfiguration.getVirtualHost());
                httpConnection.setPort(hostConfiguration.getPort());
                httpConnection.setProtocol(hostConfiguration.getProtocol());
                httpConnection.setLocalAddress(hostConfiguration.getLocalAddress());

                httpConnection.setProxyHost(hostConfiguration.getProxyHost());
                httpConnection.setProxyPort(hostConfiguration.getProxyPort());
            } else {
                finishLastResponse(httpConnection);
            }
        }

        return httpConnection;
    }

    /**
     * @see HttpConnectionManager#releaseConnection(org.apache.commons.httpclient.HttpConnection)
     */
    public void releaseConnection(HttpConnection conn) {
        if (conn != httpConnection) {
            throw new IllegalStateException("Unexpected close on a different connection.");
        }

        finishLastResponse(httpConnection);
    }

    /**
     * Since the same connection is about to be reused, make sure the
     * previous request was completely processed, and if not
     * consume it now.
     * @param conn The connection
     */
    static void finishLastResponse(HttpConnection conn) {
        InputStream lastResponse = conn.getLastResponseInputStream();
        if (lastResponse != null) {
            conn.setLastResponseInputStream(null);
            try {
                lastResponse.close();
            } catch (IOException ioe) {
                //FIXME: badness - close to force reconnect.
                conn.close();
            }
        }
    }
}
