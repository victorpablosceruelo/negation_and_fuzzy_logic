/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha3/module-client/src/main/java/org/apache/http/conn/OperatedClientConnection.java $
 * $Revision: 500781 $
 * $Date: 2007-01-28 12:48:14 +0100 (Sun, 28 Jan 2007) $
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

import java.io.IOException;
import java.net.Socket;

import org.apache.http.HttpClientConnection;
import org.apache.http.HttpHost;
import org.apache.http.HttpInetConnection;
import org.apache.http.params.HttpParams;


/**
 * A client-side connection that needs to be operated.
 * It relies on outside logic to connect sockets to the appropriate hosts.
 * It can be operated directly by an application, or through an
 * {@link ClientConnectionOperator operator}.
 *
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines to avoid svn diff problems -->
 * @version   $Revision: 500781 $ $Date: 2007-01-28 12:48:14 +0100 (Sun, 28 Jan 2007) $
 *
 * @since 4.0
 */
public interface OperatedClientConnection
    extends HttpClientConnection, HttpInetConnection {

    /**
     * Obtains the target host for this connection.
     * If the connection is to a proxy but not tunnelled, this is
     * the proxy. If the connection is tunnelled through a proxy,
     * this is the target of the tunnel.
     * <br/>
     * The return value is well-defined only while the connection is open.
     * It may change even while the connection is open,
     * because of an {@link #update update}.
     *
     * @return  the host to which this connection is opened
     */
    HttpHost getTargetHost()
        ;

    /**
     * Indicates whether this connection is secure.
     * The return value is well-defined only while the connection is open.
     * It may change even while the connection is open,
     * because of an {@link #update update}.
     *
     * @return  <code>true</code> if this connection is secure,
     *          <code>false</code> otherwise
     */
    boolean isSecure()
        ;

    /**
     * Obtains the socket for this connection.
     * The return value is well-defined only while the connection is open.
     * It may change even while the connection is open,
     * because of an {@link #update update}.
     *
     * @return  the socket for communicating with the
     *          {@link #getTargetHost target host}
     */
    Socket getSocket()
        ;


    // There is no getParams(). For the moment, we
    // do not require connections to store parameters.


    /**
     * Announces opening of this connection.
     * Opening can be announced only while the connection is closed.
     * This is an optional step, you can call {@link #open open}
     * without an announcement.
     * <br/>
     * By calling this method, you provide the connection with
     * the unconnected socket that will be connected in order
     * to call {@link #open open}. This allows the connection to
     * close that socket if
     * {@link org.apache.http.HttpConnection#shutdown shutdown}
     * is called before it is open. Closing the unconnected socket
     * will interrupt a thread that is blocked on the connect.
     * Otherwise, that thread will either time out on the connect,
     * or it returns successfully and then opens this connection
     * which was just shut down.
     * <br/>
     * <b>Note:</b>
     * The result of {@link #getSocket getSocket} is defined
     * only for open connections. You MUST NOT rely on that
     * method to return the unconnected socket after announcing.
     *
     * @param sock      the unconnected socket which is about to
     *                  be connected in order to call {@link #open open}.
     *                  <code>null</code> can be passed to undo a
     *                  previous call.
     */
    void announce(Socket sock)
        ;


    /**
     * Opens this connection.
     * A connection can be openend only while it is closed.
     * To modify a connection while it is open, use {@link #update update}.
     *
     * @param sock      the open socket for communicating with the target host
     * @param target    the target host of this connection
     * @param secure    <code>true</code> if this connection is secure, for
     *                  example if an <code>SSLSocket</code> is used, or
     *                  <code>false</code> if it is not secure
     * @param params    parameters for this connection. The parameters will
     *                  be used when creating dependent objects, for example
     *                  to determine buffer sizes.
     */
    void open(Socket sock, HttpHost target,
              boolean secure, HttpParams params)
        throws IOException
        ;


    /**
     * Updates this connection.
     * A connection can be updated only while it is open.
     * Updates are used for example when a tunnel has been established,
     * or when a TLS/SSL connection has been layered on top of a plain
     * socket connection.
     * <br/>
     * <b>Note:</b> Updating the connection will <i>not</i> close the
     * previously used socket. It is the caller's responsibility to close
     * that socket if it is no longer required.
     *
     * @param sock      the new socket for communicating with the target host,
     *                  or <code>null</code> to continue using the old socket.
     *                  If <code>null</code> is passed, helper objects that
     *                  depend on the socket should be re-used. In that case,
     *                  some changes in the parameters will not take effect.
     * @param target    the new target host of this connection
     * @param secure    <code>true</code> if this connection is now secure,
     *                  <code>false</code> if it is not secure
     * @param params    new parameters for this connection
     */
    void update(Socket sock, HttpHost target,
                boolean secure, HttpParams params)
        throws IOException
        ;


} // interface OperatedClientConnection
