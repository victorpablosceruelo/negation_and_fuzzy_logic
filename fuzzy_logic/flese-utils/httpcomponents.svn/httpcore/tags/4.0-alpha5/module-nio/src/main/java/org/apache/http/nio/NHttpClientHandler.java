/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-nio/src/main/java/org/apache/http/nio/NHttpClientHandler.java $
 * $Revision: 508428 $
 * $Date: 2007-02-16 15:52:10 +0100 (Fri, 16 Feb 2007) $
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

package org.apache.http.nio;

import java.io.IOException;

import org.apache.http.HttpException;

/**
 * Abstract client-side HTTP event handler.   
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public interface NHttpClientHandler {

    /**
     * Triggered when a new outgoing connection is created.
     * 
     * @param conn closed HTTP connection.
     * @param attachment an arbitrary object that was attached to the
     *  session request
     */
    void connected(NHttpClientConnection conn, Object attachment);
    
    /**
     * Triggered when the connection is ready to send an HTTP request.
     * 
     * @see NHttpClientConnection
     * 
     * @param conn HTTP connection that is ready to send an HTTP request
     */
    void requestReady(NHttpClientConnection conn);

    /**
     * Triggered when an HTTP response is received. The connection
     * passed as a parameter to this method is guaranteed to return
     * a valid HTTP response object.
     * <p/>
     * If the response received encloses a response entity this method will 
     * be followed a series of 
     * {@link #inputReady(NHttpClientConnection, ContentDecoder)} calls
     * to transfer the response content.
     * 
     * @see NHttpClientConnection
     * 
     * @param conn HTTP connection that contains an HTTP response
     */
    void responseReceived(NHttpClientConnection conn);
    
    /**
     * Triggered when the underlying channel is ready for reading a
     * new portion of the response entity through the corresponding 
     * content decoder. 
     * <p/>
     * If the content consumer is unable to process the incoming content,
     * input event notifications can be temorarily suspended using 
     * {@link NHttpConnection#suspendInput()}.
     * 
     * @see NHttpConnection
     * @see ContentDecoder
     *  
     * @param conn HTTP connection that can produce a new portion of the
     * incoming response content.
     * @param decoder The content decoder to use to read content.
     */
    void inputReady(NHttpClientConnection conn, ContentDecoder decoder);
    
    /**
     * Triggered when the underlying channel is ready for writing a
     * next portion of the request entity through the corresponding 
     * content encoder. 
     * <p/>
     * If the content producer is unable to generate the outgoing content,
     * output event notifications can be temorarily suspended using 
     * {@link NHttpConnection#suspendOutput()}.
     * 
     * @see NHttpConnection
     * @see ContentEncoder
     *  
     * @param conn HTTP connection that can accommodate a new portion 
     * of the outgoing request content.
     * @param encoder The content encoder to use to write content.
     */
    void outputReady(NHttpClientConnection conn, ContentEncoder encoder);
    
    /**
     * Triggered when an I/O error occurrs while reading from or writing
     * to the underlying channel.
     * 
     * @param conn HTTP connection that caused an I/O error
     * @param ex I/O exception
     */
    void exception(NHttpClientConnection conn, IOException ex);
    
    /**
     * Triggered when an HTTP protocol violation occurs while receiving 
     * an HTTP response.
     * 
     * @param conn HTTP connection that caused an HTTP protocol violation
     * @param ex HTTP protocol violation exception
     */
    void exception(NHttpClientConnection conn, HttpException ex);
    
    /**
     * Triggered when no input is detected on this connection over the
     * maximum period of inactivity.
     * 
     * @param conn HTTP connection that caused timeout condition.
     */
    void timeout(NHttpClientConnection conn);
    
    /**
     * Triggered when the connection is closed.
     * 
     * @param conn closed HTTP connection.
     */
    void closed(NHttpClientConnection conn);
    
}
