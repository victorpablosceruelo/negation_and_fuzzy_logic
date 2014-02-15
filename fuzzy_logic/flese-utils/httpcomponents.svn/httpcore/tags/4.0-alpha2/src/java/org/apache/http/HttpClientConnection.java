/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha2/src/java/org/apache/http/HttpClientConnection.java $
 * $Revision: 391251 $
 * $Date: 2006-04-04 10:52:13 +0200 (Tue, 04 Apr 2006) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2006 The Apache Software Foundation
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
 */

package org.apache.http;

import java.io.IOException;
import java.net.InetAddress;

import org.apache.http.params.HttpParams;

/**
 * An HTTP connection for use on the client side.
 * It is used for sending requests and receiving responses.
 * TODO add local port
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 391251 $
 * 
 * @since 4.0
 */
public interface HttpClientConnection extends HttpConnection {

    /**
     * Returns the current target host as set by @link #setTargetHost(HttpHost).
     * @return the target host of this connection
     */
    HttpHost getTargetHost();
    
    /**
     * Provides the implementation with the host it is supposed to connect to.
     * The host must be set prior to a call to
     * 
     * @link #open(HttpParams). The target host can only be set as long as the
     *       connection is not open.
     * @param targethost the host to connect to
     */
    void setTargetHost(HttpHost targethost);
    
    /**
     * The local address the connection is or will be bound to as set by
     * 
     * @link #setLocalAddress(InetAddress) or <code>null</code> if
     *       unspecified.
     * @return local address the connection
     */
    InetAddress getLocalAddress();
    
    /**
     * Sets the local address the connection will be bound to upon opening. The
     * local address can not only be set as long as the connection is not open.
     * If no local address is specified it is up to the implementation to
     * decide.
     * 
     * @param localAddress the local bind address or <code>null</code>.
     */
    void setLocalAddress(InetAddress localAddress);
    
    /**
     * Opens the connection. Implementations may use additional settings from
     * the HttpParams hierarchy.
     * 
     * @param params the parameters in effect for this connection.
     * @throws IOException
     */
    void open(HttpParams params) throws IOException;
    
    /**
     * Checks if response data is available from the connection. May wait for
     * the specified time until some data becomes available. Note that some
     * implementations may completely ignore the timeout parameter.
     * 
     * @param timeout the maximum time in milliseconds to wait for data
     * @return true if data is available; false if there was no data available
     *         even after waiting for <code>timeout</code> milliseconds.
     * @throws IOException if an error happens on the connection
     */
    boolean isResponseAvailable(int timeout) 
        throws IOException; 
    
    /**
     * Sends the request line and all headers over the connection.
     * @param request the request whose headers to send.
     * @throws HttpException 
     * @throws IOException
     */
    void sendRequestHeader(HttpRequest request) 
        throws HttpException, IOException;

    /**
     * Sends the request entity over the connection.
     * @param request the request whose entity to send.
     * @throws HttpException
     * @throws IOException
     */
    void sendRequestEntity(HttpEntityEnclosingRequest request) 
        throws HttpException, IOException;

    /**
     * Receives the request line and headers of the next response available from
     * this connection. The caller should examine the HttpResponse object to
     * find out if it should try to receive a response entity as well.
     * 
     * @param params the parameters in effect
     * @return a new HttpResponse object with status line and headers
     *         initialized.
     * @throws HttpException
     * @throws IOException
     */
    HttpResponse receiveResponseHeader(HttpParams params) 
        throws HttpException, IOException;

    /**
     * Receives the next response entity available from this connection and
     * attaches it to an existing HttpResponse object.
     * 
     * @param response the response to attach the entity to
     * @throws HttpException
     * @throws IOException
     */
    void receiveResponseEntity(HttpResponse response) 
        throws HttpException, IOException;
    
    /**
     * Writes out all pending buffered data over the open connection.
     * 
     * @throws IOException
     */
    void flush() throws IOException;
    
}
