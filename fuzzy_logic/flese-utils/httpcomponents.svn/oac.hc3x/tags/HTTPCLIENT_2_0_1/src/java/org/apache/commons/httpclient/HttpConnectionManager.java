/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpConnectionManager.java,v 1.15.2.1 2004/02/22 18:21:13 olegk Exp $
 * $Revision: 1.15.2.1 $
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

package org.apache.commons.httpclient;

/**
 * An interface for classes that manage HttpConnections.
 * 
 * @see org.apache.commons.httpclient.HttpConnection
 * @see org.apache.commons.httpclient.HttpClient#HttpClient(HttpConnectionManager)
 *
 * @author Unascribed
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * 
 * @since 2.0
 */
public interface HttpConnectionManager {

    /**
     * Gets an HttpConnection for a given host configuration. If a connection is
     * not available this method will block until one is.
     *
     * The connection manager should be registered with any HttpConnection that
     * is created.
     *
     * @param hostConfiguration the host configuration to use to configure the
     * connection
     * 
     * @return an HttpConnection for the given configuration
     * 
     * @see HttpConnection#setHttpConnectionManager(HttpConnectionManager)
     */
    HttpConnection getConnection(HostConfiguration hostConfiguration);

    /**
     * Gets an HttpConnection for a given host configuration. If a connection is
     * not available, this method will block for at most the specified number of
     * milliseconds or until a connection becomes available.
     *
     * The connection manager should be registered with any HttpConnection that
     * is created.
     *
     * @param hostConfiguration the host configuration to use to configure the
     * connection
     * @param timeout - the time (in milliseconds) to wait for a connection to
     * become available, 0 to specify an infinite timeout
     * 
     * @return an HttpConnection for the given configuraiton
     * 
     * @throws HttpException if no connection becomes available before the
     * timeout expires
     * 
     * @see HttpConnection#setHttpConnectionManager(HttpConnectionManager)
     */
    HttpConnection getConnection(HostConfiguration hostConfiguration, long timeout)
        throws HttpException;

    /**
     * Releases the given HttpConnection for use by other requests.
     *
     * @param conn - The HttpConnection to make available.
     */
    void releaseConnection(HttpConnection conn);
}
