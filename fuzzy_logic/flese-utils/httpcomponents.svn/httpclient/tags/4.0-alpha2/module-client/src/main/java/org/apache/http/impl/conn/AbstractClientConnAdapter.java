/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha2/module-client/src/main/java/org/apache/http/impl/conn/AbstractClientConnAdapter.java $
 * $Revision: 580608 $
 * $Date: 2007-09-29 19:25:18 +0200 (Sat, 29 Sep 2007) $
 *
 * ====================================================================
 *
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
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
 */

package org.apache.http.impl.conn;


import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSession;

import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpConnectionMetrics;
import org.apache.http.conn.OperatedClientConnection;
import org.apache.http.conn.ManagedClientConnection;
import org.apache.http.conn.ClientConnectionManager;


/**
 * Abstract adapter from {@link OperatedClientConnection operated} to
 * {@link ManagedClientConnection managed} client connections.
 * Read and write methods are delegated to the wrapped connection.
 * Operations affecting the connection state have to be implemented
 * by derived classes. Operations for querying the connection state
 * are delegated to the wrapped connection if there is one, or
 * return a default value if there is none.
 * <br/>
 * This adapter tracks the checkpoints for reusable communication states,
 * as indicated by {@link #markReusable markReusable} and queried by
 * {@link #isMarkedReusable isMarkedReusable}.
 * All send and receive operations will automatically clear the mark.
 * <br/>
 * Connection release calls are delegated to the connection manager,
 * if there is one. {@link #abortConnection abortConnection} will
 * clear the reusability mark first. The connection manager is
 * expected to tolerate multiple calls to the release method.
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines to avoid svn diff problems -->
 * @version   $Revision: 580608 $ $Date: 2007-09-29 19:25:18 +0200 (Sat, 29 Sep 2007) $
 *
 * @since 4.0
 */
public abstract class AbstractClientConnAdapter
    implements ManagedClientConnection {

    /**
     * The connection manager, if any.
     * This attribute MUST NOT be final, so the adapter can be detached
     * from the connection manager without keeping a hard reference there.
     */
    protected ClientConnectionManager connManager;

    /** The wrapped connection. */
    protected OperatedClientConnection wrappedConnection;

    /** The reusability marker. */
    protected boolean markedReusable;


    /**
     * Creates a new connection adapter.
     * The adapter is initially <i>not</i>
     * {@link #isMarkedReusable marked} as reusable.
     *
     * @param mgr       the connection manager, or <code>null</code>
     * @param conn      the connection to wrap, or <code>null</code>
     */
    protected AbstractClientConnAdapter(ClientConnectionManager mgr,
                                        OperatedClientConnection conn) {

        connManager = mgr;
        wrappedConnection = conn;
        markedReusable = false;

    } // <constructor>


    /**
     * Asserts that there is a wrapped connection to delegate to.
     *
     * @throws IllegalStateException    if there is no wrapped connection
     */
    protected final void assertWrappedConn() {
        if (wrappedConnection == null) {
            throw new IllegalStateException("No wrapped connection.");
        }
    }


    // non-javadoc, see interface HttpConnection
    public boolean isOpen() {
        if (wrappedConnection == null)
            return false;

        return wrappedConnection.isOpen();
    }


    // non-javadoc, see interface HttpConnection
    public boolean isStale() {
        if (wrappedConnection == null)
            return true;

        return wrappedConnection.isStale();
    }


    // non-javadoc, see interface HttpConnection
    public void setSocketTimeout(int timeout) {
        assertWrappedConn();
        wrappedConnection.setSocketTimeout(timeout);
    }


    // non-javadoc, see interface HttpConnection
    public int getSocketTimeout() {
        assertWrappedConn();
        return wrappedConnection.getSocketTimeout();
    }


    // non-javadoc, see interface HttpConnection
    public HttpConnectionMetrics getMetrics() {
        assertWrappedConn();
        return wrappedConnection.getMetrics();
    }


    // non-javadoc, see interface HttpClientConnection
    public void flush()
        throws IOException {

        assertWrappedConn();
        wrappedConnection.flush();
    }


    // non-javadoc, see interface HttpClientConnection
    public boolean isResponseAvailable(int timeout)
        throws IOException {

        assertWrappedConn();
        return wrappedConnection.isResponseAvailable(timeout);
    }


    // non-javadoc, see interface HttpClientConnection
    public void receiveResponseEntity(HttpResponse response)
        throws HttpException, IOException {

        assertWrappedConn();
        markedReusable = false;
        wrappedConnection.receiveResponseEntity(response);
    }


    // non-javadoc, see interface HttpClientConnection
    public HttpResponse receiveResponseHeader()
        throws HttpException, IOException {

        assertWrappedConn();
        markedReusable = false;
        return wrappedConnection.receiveResponseHeader();
    }


    // non-javadoc, see interface HttpClientConnection
    public void sendRequestEntity(HttpEntityEnclosingRequest request)
        throws HttpException, IOException {

        assertWrappedConn();
        markedReusable = false;
        wrappedConnection.sendRequestEntity(request);
    }


    // non-javadoc, see interface HttpClientConnection
    public void sendRequestHeader(HttpRequest request)
        throws HttpException, IOException {

        assertWrappedConn();
        markedReusable = false;
        wrappedConnection.sendRequestHeader(request);
    }


    // non-javadoc, see interface HttpInetConnection
    public InetAddress getLocalAddress() {
        assertWrappedConn();
        return wrappedConnection.getLocalAddress();
    }

    // non-javadoc, see interface HttpInetConnection
    public int getLocalPort() {
        assertWrappedConn();
        return wrappedConnection.getLocalPort();
    }


    // non-javadoc, see interface HttpInetConnection
    public InetAddress getRemoteAddress() {
        assertWrappedConn();
        return wrappedConnection.getRemoteAddress();
    }

    // non-javadoc, see interface HttpInetConnection
    public int getRemotePort() {
        assertWrappedConn();
        return wrappedConnection.getRemotePort();
    }

    // non-javadoc, see interface ManagedClientConnection
    public boolean isSecure() {
        assertWrappedConn();
        return wrappedConnection.isSecure();
    }

    // non-javadoc, see interface ManagedClientConnection
    public SSLSession getSSLSession() {
        if (!isOpen())
            return null;

        SSLSession result = null;
        Socket    sock    = wrappedConnection.getSocket();
        if (sock instanceof SSLSocket) {
            result = ((SSLSocket)sock).getSession();
        }
        return result;
    }

    // non-javadoc, see interface ManagedClientConnection
    public void markReusable() {
        markedReusable = true;
    }

    // non-javadoc, see interface ManagedClientConnection
    public void unmarkReusable() {
        markedReusable = false;
    }

    // non-javadoc, see interface ManagedClientConnection
    public boolean isMarkedReusable() {
        return markedReusable;
    }

    // non-javadoc, see interface ConnectionReleaseTrigger
    public void releaseConnection() {
        if (connManager != null)
            connManager.releaseConnection(this);
    }

    // non-javadoc, see interface ConnectionReleaseTrigger
    public void abortConnection() {

        unmarkReusable();

        if (connManager != null)
            connManager.releaseConnection(this);
    }

} // class AbstractClientConnAdapter
