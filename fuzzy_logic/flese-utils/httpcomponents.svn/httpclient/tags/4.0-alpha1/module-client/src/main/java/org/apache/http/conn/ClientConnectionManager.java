/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/main/java/org/apache/http/conn/ClientConnectionManager.java $
 * $Revision: 534769 $
 * $Date: 2007-05-03 11:51:22 +0200 (Thu, 03 May 2007) $
 *
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.http.conn;



/**
 * Management interface for {@link ManagedClientConnection client connections}.
 * 
 * @author Michael Becke
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines to avoid svn diff problems -->
 * @version   $Revision: 534769 $
 *
 * @since 4.0
 */
public interface ClientConnectionManager {

    SchemeRegistry getSchemeRegistry();
    
    /**
     * Obtains a connection.
     * This method will block until a connection becomes available
     * or the connection manager is {@link #shutdown shut down}.
     *
     * @param route     where the connection should point to
     *
     * @return  a connection that can be used to communicate
     *          along the given route
     */
    ManagedClientConnection getConnection(HttpRoute route)
        ;


    /**
     * Obtains a connection within a given time.
     * This method will block until a connection becomes available,
     * the timeout expires, or the connection manager is
     * {@link #shutdown shut down}.
     *
     * @param route     where the connection should point to
     * @param timeout   the timeout in milliseconds
     *
     * @return  a connection that can be used to communicate
     *          along the given route
     *
     * @throws ConnectionPoolTimeoutException
     *          in case of a timeout
     */
    ManagedClientConnection getConnection(HttpRoute route,
                                          long timeout)
        throws ConnectionPoolTimeoutException
        ;


    /**
     * Releases a connection for use by others.
     * If the argument connection has been released before,
     * the call will be ignored.
     *
     * @param conn      the connection to release
     */
    void releaseConnection(ManagedClientConnection conn)
        ;


    /**
     * Closes idle connections in the pool.
     * Open connections in the pool that have not been used for the
     * timespan given by the timeout argument will be closed.
     * Currently allocated connections are not subject to this method.
     * 
     * @param idletime       the idle time in milliseconds
     */
    void closeIdleConnections(long idletime)
        ;


    /**
     * Shuts down this connection manager and releases allocated resources.
     * This includes closing all connections, whether they are currently
     * used or not.
     */
    void shutdown()
        ;


} // interface ClientConnectionManager
