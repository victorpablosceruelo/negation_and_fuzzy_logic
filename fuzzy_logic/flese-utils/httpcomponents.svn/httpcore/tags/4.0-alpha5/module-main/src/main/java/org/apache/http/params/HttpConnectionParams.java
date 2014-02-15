/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-main/src/main/java/org/apache/http/params/HttpConnectionParams.java $
 * $Revision: 541979 $
 * $Date: 2007-05-27 14:50:50 +0200 (Sun, 27 May 2007) $
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

package org.apache.http.params;

/**
 * An adaptor for accessing connection parameters in {@link HttpParams}.
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 541979 $
 * 
 * @since 4.0
 */
public final class HttpConnectionParams {

    /**
     * Defines the default socket timeout (<tt>SO_TIMEOUT</tt>) in milliseconds which is the 
     * timeout for waiting for data. A timeout value of zero is interpreted as an infinite 
     * timeout. This value is used when no socket timeout is set in the 
     * method parameters. 
     * <p>
     * This parameter expects a value of type {@link Integer}.
     * </p>
     * @see java.net.SocketOptions#SO_TIMEOUT
     */
    public static final String SO_TIMEOUT = "http.socket.timeout"; 

    /**
     * Determines whether Nagle's algorithm is to be used. The Nagle's algorithm 
     * tries to conserve bandwidth by minimizing the number of segments that are 
     * sent. When applications wish to decrease network latency and increase 
     * performance, they can disable Nagle's algorithm (that is enable TCP_NODELAY). 
     * Data will be sent earlier, at the cost of an increase in bandwidth consumption. 
     * <p>
     * This parameter expects a value of type {@link Boolean}.
     * </p>
     * @see java.net.SocketOptions#TCP_NODELAY
     */
    public static final String TCP_NODELAY = "http.tcp.nodelay"; 

    /**
     * Determines the size of the internal socket buffer used to buffer data
     * while receiving / transmitting HTTP messages.
     * <p>
     * This parameter expects a value of type {@link Integer}.
     * </p>
     */
    public static final String SOCKET_BUFFER_SIZE = "http.socket.buffer-size"; 

    /**
     * Sets SO_LINGER with the specified linger time in seconds. The maximum timeout 
     * value is platform specific. Value <tt>0</tt> implies that the option is disabled.
     * Value <tt>-1</tt> implies that the JRE default is used. The setting only affects 
     * socket close.  
     * <p>
     * This parameter expects a value of type {@link Integer}.
     * </p>
     * @see java.net.SocketOptions#SO_LINGER
     */
    public static final String SO_LINGER = "http.socket.linger"; 

    /**
     * Determines the timeout until a connection is etablished. A value of zero 
     * means the timeout is not used. The default value is zero.
     * <p>
     * This parameter expects a value of type {@link Integer}.
     * </p>
     */
    public static final String CONNECTION_TIMEOUT = "http.connection.timeout"; 

    /**
     * Determines whether stale connection check is to be used. Disabling 
     * stale connection check may result in slight performance improvement 
     * at the risk of getting an I/O error when executing a request over a
     * connection that has been closed at the server side. 
     * <p>
     * This parameter expects a value of type {@link Boolean}.
     * </p>
     */
    public static final String STALE_CONNECTION_CHECK = "http.connection.stalecheck"; 

    /**
     * Determines the maximum line length limit. If set to a positive value, any HTTP 
     * line exceeding this limit will cause an IOException. A negative or zero value
     * will effectively disable the check.
     * <p>
     * This parameter expects a value of type {@link Integer}.
     * </p>
     */
    public static final String MAX_LINE_LENGTH = "http.connection.max-line-length";
    
    /**
     * Determines the maximum HTTP header count allowed. If set to a positive value, 
     * the number of HTTP headers received from the data stream exceeding this limit 
     * will cause an IOException. A negative or zero value will effectively disable 
     * the check. 
     * <p>
     * This parameter expects a value of type {@link Integer}.
     * </p>
     */
    public static final String MAX_HEADER_COUNT = "http.connection.max-header-count";
    
    /**
     * Defines the maximum number of ignorable lines before we expect
     * a HTTP response's status code.
     * <p>
     * With HTTP/1.1 persistent connections, the problem arises that
     * broken scripts could return a wrong Content-Length
     * (there are more bytes sent than specified).<br />
     * Unfortunately, in some cases, this is not possible after the bad response,
     * but only before the next one. <br />
     * So, HttpClient must be able to skip those surplus lines this way.
     * </p>
     * <p>
     * Set this to 0 to disallow any garbage/empty lines before the status line.<br />
     * To specify no limit, use {@link java.lang.Integer#MAX_VALUE} (default in lenient mode).
     * </p>
     *  
     * This parameter expects a value of type {@link Integer}.
     */
    public static final String MAX_STATUS_LINE_GARBAGE = "http.connection.max-status-line-garbage";

    /**
     */
    private HttpConnectionParams() {
        super();
    }

    /**
     * Returns the default socket timeout (<tt>SO_TIMEOUT</tt>) in milliseconds which is the 
     * timeout for waiting for data. A timeout value of zero is interpreted as an infinite 
     * timeout. This value is used when no socket timeout is set in the 
     * method parameters. 
     *
     * @return timeout in milliseconds
     */
    public static int getSoTimeout(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getIntParameter(SO_TIMEOUT, 0);
    }

    /**
     * Sets the default socket timeout (<tt>SO_TIMEOUT</tt>) in milliseconds which is the 
     * timeout for waiting for data. A timeout value of zero is interpreted as an infinite 
     * timeout. This value is used when no socket timeout is set in the 
     * method parameters. 
     *
     * @param timeout Timeout in milliseconds
     */
    public static void setSoTimeout(final HttpParams params, int timeout) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setIntParameter(SO_TIMEOUT, timeout);
        
    }

    /**
     * Tests if Nagle's algorithm is to be used.  
     *
     * @return <tt>true</tt> if the Nagle's algorithm is to NOT be used
     *   (that is enable TCP_NODELAY), <tt>false</tt> otherwise.
     */
    public static boolean getTcpNoDelay(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getBooleanParameter(TCP_NODELAY, true);
    }

    /**
     * Determines whether Nagle's algorithm is to be used. The Nagle's algorithm 
     * tries to conserve bandwidth by minimizing the number of segments that are 
     * sent. When applications wish to decrease network latency and increase 
     * performance, they can disable Nagle's algorithm (that is enable TCP_NODELAY). 
     * Data will be sent earlier, at the cost of an increase in bandwidth consumption. 
     *
     * @param value <tt>true</tt> if the Nagle's algorithm is to NOT be used
     *   (that is enable TCP_NODELAY), <tt>false</tt> otherwise.
     */
    public static void setTcpNoDelay(final HttpParams params, boolean value) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setBooleanParameter(TCP_NODELAY, value);
    }

    public static int getSocketBufferSize(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getIntParameter(SOCKET_BUFFER_SIZE, -1);
    }
    
    public static void setSocketBufferSize(final HttpParams params, int size) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setIntParameter(SOCKET_BUFFER_SIZE, size);
    }

    /**
     * Returns linger-on-close timeout. Value <tt>0</tt> implies that the option is 
     * disabled. Value <tt>-1</tt> implies that the JRE default is used.
     * 
     * @return the linger-on-close timeout
     */
    public static int getLinger(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getIntParameter(SO_LINGER, -1);
    }

    /**
     * Returns linger-on-close timeout. This option disables/enables immediate return 
     * from a close() of a TCP Socket. Enabling this option with a non-zero Integer 
     * timeout means that a close() will block pending the transmission and 
     * acknowledgement of all data written to the peer, at which point the socket is 
     * closed gracefully. Value <tt>0</tt> implies that the option is 
     * disabled. Value <tt>-1</tt> implies that the JRE default is used.
     *
     * @param value the linger-on-close timeout
     */
    public static void setLinger(final HttpParams params, int value) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setIntParameter(SO_LINGER, value);
    }

    /**
     * Returns the timeout until a connection is etablished. A value of zero 
     * means the timeout is not used. The default value is zero.
     * 
     * @return timeout in milliseconds.
     */
    public static int getConnectionTimeout(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getIntParameter(CONNECTION_TIMEOUT, 0);
    }

    /**
     * Sets the timeout until a connection is etablished. A value of zero 
     * means the timeout is not used. The default value is zero.
     * 
     * @param timeout Timeout in milliseconds.
     */
    public static void setConnectionTimeout(final HttpParams params, int timeout) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setIntParameter(CONNECTION_TIMEOUT, timeout);
    }
    
    /**
     * Tests whether stale connection check is to be used. Disabling 
     * stale connection check may result in slight performance improvement 
     * at the risk of getting an I/O error when executing a request over a
     * connection that has been closed at the server side. 
     * 
     * @return <tt>true</tt> if stale connection check is to be used, 
     *   <tt>false</tt> otherwise.
     */
    public static boolean isStaleCheckingEnabled(final HttpParams params) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        return params.getBooleanParameter(STALE_CONNECTION_CHECK, true);
    }

    /**
     * Defines whether stale connection check is to be used. Disabling 
     * stale connection check may result in slight performance improvement 
     * at the risk of getting an I/O error when executing a request over a
     * connection that has been closed at the server side. 
     * 
     * @param value <tt>true</tt> if stale connection check is to be used, 
     *   <tt>false</tt> otherwise.
     */
    public static void setStaleCheckingEnabled(final HttpParams params, boolean value) {
        if (params == null) {
            throw new IllegalArgumentException("HTTP parameters may not be null");
        }
        params.setBooleanParameter(STALE_CONNECTION_CHECK, value);
    }
    
}
