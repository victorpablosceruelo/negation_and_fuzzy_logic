/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpConnection.java,v 1.45 2003/02/16 13:08:32 olegk Exp $
 * $Revision: 1.45 $
 * $Date: 2003-02-16 14:10:16 +0100 (Sun, 16 Feb 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
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
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.Socket;
import java.net.SocketException;

import org.apache.commons.httpclient.protocol.DefaultProtocolSocketFactory;
import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.httpclient.protocol.ProtocolSocketFactory;
import org.apache.commons.httpclient.protocol.SecureProtocolSocketFactory;
import org.apache.commons.httpclient.util.TimeoutController;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * An abstraction of an HTTP {@link InputStream} and {@link OutputStream}
 * pair, together with the relevant attributes.
 * <p>
 * The following options are set on the socket before getting the input/output 
 * streams in the {@link #open()} method:
 * <table border=1><tr>
 *    <th>Socket Method
 *    <th>Sockets Option
 *    <th>Configuration
 * </tr><tr>
 *    <td>{@link java.net.Socket#setTcpNoDelay(boolean)}
 *    <td>SO_NODELAY
 *    <td>None
 * </tr><tr>
 *    <td>{@link java.net.Socket#setSoTimeout(int)}
 *    <td>SO_TIMEOUT
 *    <td>{@link #setConnectionTimeout(int)}
 * </tr></table>
 *
 * @author Rod Waldhoff
 * @author Sean C. Sullivan
 * @author Ortwin Glück
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * @author Michael Becke
 * 
 * @version   $Revision: 1.45 $ $Date: 2003-02-16 14:10:16 +0100 (Sun, 16 Feb 2003) $
 */
public class HttpConnection {

    // ----------------------------------------------------------- Constructors

    /**
     * Constructor.
     *
     * @param host the host I should connect to
     * @param port the port I should connect to
     */
    public HttpConnection(String host, int port) {
        this(null, -1, host, port, false);
    }

    /**
     * Constructor.
     *
     * @param host the host I should connect to
     * @param port the port I should connect to
     * @param secure when <tt>true</tt>, connect via HTTPS (SSL)
     * 
     * @deprecated use HttpConnection(String, int, Protocol)
     * 
     * @see #HttpConnection(String,int,Protocol)
     *
     */
    public HttpConnection(String host, int port, boolean secure) {
        this(null, -1, host, port, secure);
    }

    /**
     * Constructor.
     *
     * @param host the host I should connect to
     * @param port the port I should connect to
     * @param protocol the protocol to use
     */
    public HttpConnection(String host, int port, Protocol protocol) {
        this(null, -1, host, port, protocol);
    }

    /**
     * Constructor.
     *
     * @param proxyHost the host I should proxy via
     * @param proxyPort the port I should proxy via
     * @param host the host I should connect to
     * @param port the port I should connect to
     */
    public HttpConnection(
        String proxyHost,
        int proxyPort,
        String host,
        int port) {
        this(proxyHost, proxyPort, host, port, false);
    }

    /**
     * Fully-specified constructor.
     *
     * @param proxyHost the host I should proxy via
     * @param proxyPort the port I should proxy via
     * @param host the host I should connect to. Parameter value must be non-null.
     * @param port the port I should connect to
     * @param secure when <tt>true</tt>, connect via HTTPS (SSL)
     * 
     * @deprecated use HttpConnection(String, int, String, int, Protocol)
     * 
     * @see #HttpConnection(String, int, String, int, Protocol)
     *  
     */
    public HttpConnection(
        String proxyHost,
        int proxyPort,
        String host,
        int port,
        boolean secure) {
        this(proxyHost, proxyPort, host, port,
            Protocol.getProtocol(secure ? "https" : "http"));
    }

    /**
     * Creates a new HttpConnection.
     * 
     * @param hostConfiguration the host/proxy/protocol to use
     */
    public HttpConnection(HostConfiguration hostConfiguration) {
        this(hostConfiguration.getProxyHost(),
             hostConfiguration.getProxyPort(),
             hostConfiguration.getHost(),
             hostConfiguration.getPort(),
             hostConfiguration.getProtocol());
    }

    /**
     * Create an instance
     * 
     * @param proxyHost the host I should proxy via
     * @param proxyPort the port I should proxy via
     * @param host the host I should connect to. Parameter value must be non-null.
     * @param port the port I should connect to
     * @param protocol The protocol to use.
     */
    public HttpConnection(
        String proxyHost,
        int proxyPort,
        String host,
        int port,
        Protocol protocol) {

        if (LOG.isDebugEnabled()) {
            LOG.debug("HttpConnectionManager.getConnection:  creating  connection for "
                    + host + ":" + port + " via " + proxyHost + ":" + proxyPort
                    + " using protocol: " + protocol);
        }

        if (host == null) {
            throw new IllegalArgumentException("host parameter is null");
        }
        if (protocol == null) {
            throw new IllegalArgumentException("protocol is null");
        }

        proxyHostName = proxyHost;
        proxyPortNumber = proxyPort;
        hostName = host;
        portNumber = protocol.resolvePort(port);
        protocolInUse = protocol;

    }

    // ------------------------------------------ Attribute Setters and Getters

    /**
     * Return my host.
     *
     * @return my host.
     */
    public String getHost() {
        return hostName;
    }

    /**
     * Set my host.
     *
     * @param host the host I should connect to. Parameter value must be non-null.
     * @throws IllegalStateException if I am already connected
     */
    public void setHost(String host) throws IllegalStateException {
        if (host == null) {
            throw new NullPointerException("host parameter is null");
        }
        assertNotOpen();
        hostName = host;
    }

    /**
     * Return my port.
     *
     * If the port is -1 (or less than 0) the default port for
     * the current protocol is returned.
     *
     * @return my port.
     */
    public int getPort() {
        if (portNumber < 0) {
            return isSecure() ? 443 : 80;
        } else {
            return portNumber;
        }
    }

    /**
     * Set my port.
     *
     * @param port the port I should connect to
     * @throws IllegalStateException if I am already connected
     */
    public void setPort(int port) throws IllegalStateException {
        assertNotOpen();
        portNumber = port;
    }

    /**
     * Return my proxy host.
     *
     * @return my proxy host.
     */
    public String getProxyHost() {
        return proxyHostName;
    }

    /**
     * Set the host I should proxy through.
     *
     * @param host the host I should proxy through.
     * @throws IllegalStateException if I am already connected
     */
    public void setProxyHost(String host) throws IllegalStateException {
        assertNotOpen();
        proxyHostName = host;
    }

    /**
     * Return my proxy port.
     *
     * @return my proxy port.
     */
    public int getProxyPort() {
        return proxyPortNumber;
    }

    /**
     * Set the port I should proxy through.
     *
     * @param port the host I should proxy through.
     * @throws IllegalStateException if I am already connected
     */
    public void setProxyPort(int port) throws IllegalStateException {
        assertNotOpen();
        proxyPortNumber = port;
    }

    /**
     * Return <tt>true</tt> if I will (or I am) connected over a
     * secure (HTTPS/SSL) protocol.
     *
     * @return <tt>true</tt> if I will (or I am) connected over a
     *         secure (HTTPS/SSL) protocol.
     */
    public boolean isSecure() {
        return protocolInUse.isSecure();
    }

    /**
     * Get the protocol.
     * @return HTTPS if secure, HTTP otherwise
     */
    public Protocol getProtocol() {
        return protocolInUse;
    }

    /**
     * Set whether or not I should connect over HTTPS (SSL).
     *
     * @param secure whether or not I should connect over HTTPS (SSL).
     * @throws IllegalStateException if I am already connected
     * 
     * @deprecated use setProtocol(Protocol)
     * 
     * @see #setProtocol(Protocol)
     */
    public void setSecure(boolean secure) throws IllegalStateException {
        assertNotOpen();
        protocolInUse = secure
            ? Protocol.getProtocol("https")
            : Protocol.getProtocol("http");
    }

    /**
     * Sets the protocol used by this connection.
     * 
     * @param protocol The new protocol.
     */
    public void setProtocol(Protocol protocol) {
        assertNotOpen();

        if (protocol == null) {
            throw new IllegalArgumentException("protocol is null");
        }

        protocolInUse = protocol;

    }

    /**
     * Return <tt>true</tt> if I am connected,
     * <tt>false</tt> otherwise.
     *
     * @return <tt>true</tt> if I am connected
     */
    public boolean isOpen() {
        return isOpen;
    }

    /**
     * Return <tt>true</tt> if I am (or I will be)
     * connected via a proxy, <tt>false</tt> otherwise.
     *
     * @return <tt>true</tt> if I am (or I will be)
     *         connected via a proxy, <tt>false</tt> otherwise.
     */
    public boolean isProxied() {
        return (!(null == proxyHostName || 0 >= proxyPortNumber));
    }

    /**
     * Set the state to keep track of the last response for the last request.
     *
     * <p>The connection managers use this to ensure that previous requests are
     * properly closed before a new request is attempted.  That way, a GET
     * request need not be read in its entirety before a new request is issued.
     * Instead, this stream can be closed as appropriate.</p>
     *
     * @param inStream  The stream associated with an HttpMethod.
     */
    public void setLastResponseInputStream(InputStream inStream) {
        lastResponseInputStream = inStream;
    }

    /**
     * Returns the stream used to read the last response's body.
     *
     * <p>Clients will generally not need to call this function unless
     * using HttpConnection directly, instead of calling {@link HttpClient#executeMethod}.
     * For those clients, call this function, and if it returns a non-null stream,
     * close the stream before attempting to execute a method.  Note that
     * calling "close" on the stream returned by this function <i>may</i> close
     * the connection if the previous response contained a "Connection: close" header. </p>
     *
     * @return An {@link InputStream} corresponding to the body of the last
     *  response.
     */
    public InputStream getLastResponseInputStream() {
        return lastResponseInputStream;
    }

    // --------------------------------------------------- Other Public Methods

    /**
     * Set my {@link Socket}'s timeout, via {@link Socket#setSoTimeout}.  If the
     * connection is already open, the SO_TIMEOUT is changed.  If no connection
     * is open, then subsequent connections will use the timeout value.
     * <p>
     * Note: This is not a connection timeout but a timeout on network traffic!
     *
     * @param timeout the timeout value
     * @throws SocketException - if there is an error in the underlying
     * protocol, such as a TCP error.
     * @throws IllegalStateException if I am not connected
     */
    public void setSoTimeout(int timeout)
        throws SocketException, IllegalStateException {
        LOG.debug("HttpConnection.setSoTimeout(" + timeout + ")");
        soTimeout = timeout;
        if (socket != null) {
            socket.setSoTimeout(timeout);
        }
    }

    /**
     * Sets the connection timeout. This is the maximum time that may be spent
     * until a connection is established. The connection will fail after this
     * amount of time.
     * @param timeout The timeout in milliseconds. 0 means timeout is not used.
     */
    public void setConnectionTimeout(int timeout) {
        this.connectTimeout = timeout;
    }

    /**
     * Open this connection to the current host and port
     * (via a proxy if so configured).
     * The underlying socket is created from the {@link ProtocolSocketFactory}.
     *
     * @throws IOException when there are errors opening the connection
     */
    public void open() throws IOException {
        LOG.trace("enter HttpConnection.open()");

        assertNotOpen(); // ??? is this worth doing?
        try {
            if (null == socket) {

                final String host = (null == proxyHostName) ? hostName : proxyHostName;
                final int port = (null == proxyHostName) ? portNumber : proxyPortNumber;

                usingSecureSocket = isSecure() && !isProxied();

                final ProtocolSocketFactory socketFactory =
                    (isSecure()
                        && !isProxied()
                            ? protocolInUse.getSocketFactory()
                            : new DefaultProtocolSocketFactory());

                if (connectTimeout == 0) {
                    socket = socketFactory.createSocket(host, port);
                } else {
                    SocketTask task = new SocketTask() {
                        public void doit() throws IOException {
                            setSocket(socketFactory.createSocket(host, port));
                        }
                    };
                    TimeoutController.execute(task, connectTimeout);
                    socket = task.getSocket();
                    if (task.exception != null) {
                        throw task.exception;
                    }
                }

            }

            /*
            "Nagling has been broadly implemented across networks, 
            including the Internet, and is generally performed by default 
            - although it is sometimes considered to be undesirable in 
            highly interactive environments, such as some client/server 
            situations. In such cases, nagling may be turned off through 
            use of the TCP_NODELAY sockets option." */

            socket.setTcpNoDelay(soNodelay);
            socket.setSoTimeout(soTimeout);
            inputStream = socket.getInputStream();
            outputStream = socket.getOutputStream();
            isOpen = true;
        } catch (IOException e) {
            // Connection wasn't opened properly
            // so close everything out
            closeSocketAndStreams();
            throw e;
        } catch (TimeoutController.TimeoutException e) {
            if (LOG.isWarnEnabled()) {
                LOG.warn( "The host " + hostName + ":" + portNumber
                        + " (or proxy " + proxyHostName + ":" + proxyPortNumber
                        + ") did not accept the connection within timeout of "
                        + connectTimeout + " milliseconds");
            }
            throw new ConnectionTimeoutException();
        }
    }

    /**
     * Calling this method indicates that the proxy has successfully created the
     * tunnel to the host. The socket will be switched to the secure socket.
     * Subsequent communication is done via the secure socket. The method can
     * only be called once on a proxied secure connection.
     *
     * @throws IllegalStateException if connection is not secure and proxied or
     * if the socket is already secure.
     * @throws IOException if an error occured creating the secure socket
     */
    public void tunnelCreated() throws IllegalStateException, IOException {
        LOG.trace("enter HttpConnection.tunnelCreated()");

        if (!isSecure() || !isProxied()) {
            throw new IllegalStateException(
                "Connection must be secure "
                    + "and proxied to use this feature");
        }

        if (usingSecureSocket) {
            throw new IllegalStateException("Already using a secure socket");
        }

        SecureProtocolSocketFactory socketFactory =
            (SecureProtocolSocketFactory) protocolInUse.getSocketFactory();

        socket = socketFactory.createSocket(socket, hostName, portNumber, true);
        inputStream = socket.getInputStream();
        outputStream = socket.getOutputStream();
        usingSecureSocket = true;
        tunnelEstablished = true;
        LOG.debug("Secure tunnel created");
    }

    /**
     * Indicates if the connection is completely transparent from end to end.
     *
     * @return true if conncetion is not proxied or tunneled through a transparent
     * proxy; false otherwise.
     */
    public boolean isTransparent() {
        return !isProxied() || tunnelEstablished;
    }

    /**
     * Return a {@link RequestOutputStream} suitable for writing (possibly
     * chunked) bytes to my {@link OutputStream}.
     *
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     * @return a stream to write the request to
     */
    public OutputStream getRequestOutputStream()
        throws IOException, IllegalStateException {
        LOG.trace("enter HttpConnection.getRequestOutputStream()");
        assertOpen();
        return outputStream;
    }

    /**
     * Return a {@link RequestOutputStream} suitable for writing (possibly
     * chunked) bytes to my {@link OutputStream}.
     *
     * @param useChunking when <tt>true</tt> the chunked transfer-encoding will
     *      be used
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     * @return a stream to write the request to
     * @deprecated Use new ChunkedOutputStream(httpConnecion.getRequestOutputStream());
     */
    public OutputStream getRequestOutputStream(boolean useChunking)
        throws IOException, IllegalStateException {
        LOG.trace("enter HttpConnection.getRequestOutputStream(boolean)");

        assertOpen();
        if (useChunking) {
            return new ChunkedOutputStream(outputStream);
        } else {
            return outputStream;
        }
    }

    /**
     * Return a {@link ResponseInputStream} suitable for reading (possibly
     * chunked) bytes from my {@link InputStream}.
     * <p>
     * If the given {@link HttpMethod} contains
     * a <tt>Transfer-Encoding: chunked</tt> header,
     * the returned stream will be configured
     * to read chunked bytes.
     *
     * @param method This argument is ignored.
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     * @return a stream to read the response from
     * @deprecated Use getResponseInputStream() instead.
     */
    public InputStream getResponseInputStream(HttpMethod method)
        throws IOException, IllegalStateException {
        LOG.trace("enter HttpConnection.getResponseInputStream(HttpMethod)");
        return getResponseInputStream();
    }

    /**
     * Return the response input stream
     * @return InputStream The response input stream.
     * @throws IOException If an IO problem occurs
     * @throws IllegalStateException If the connection isn't open.
     */
    public InputStream getResponseInputStream()
        throws IOException, IllegalStateException {
        LOG.trace("enter HttpConnection.getResponseInputStream()");
        assertOpen();
        return inputStream;
    }

    /**
     * Tests if input data avaialble.
     * 
     * @return boolean <tt>true</tt> if input data is availble, 
     *                 <tt>false</tt> otherwise.
     * 
     * @throws IOException If an IO problem occurs
     * @throws IllegalStateException If the connection isn't open.
     */
    public boolean isResponseAvaliable() 
        throws IOException {
        LOG.trace("enter HttpConnection.responseAvaliable()");
        assertOpen();
        return this.inputStream.available() > 0;
    }


    /**
     * Waits for the specified number of milliseconds for input data to become available
     * 
     * @param timeout_ms Number of milliseconds to wait for input data.
     * 
     * @return boolean <tt>true</tt> if input data is availble, 
     *                 <tt>false</tt> otherwise.
     * 
     * @throws IOException If an IO problem occurs
     * @throws IllegalStateException If the connection isn't open.
     */
    public boolean waitForResponse(long timeout_ms)
        throws IOException, IllegalStateException {
        LOG.trace("enter HttpConnection.waitForResponse(int)");
        if (timeout_ms < 0) {
            throw new IllegalArgumentException("Timeout value may not be negative");
        }
        long overtime = System.currentTimeMillis() + timeout_ms;
        while (System.currentTimeMillis() < overtime) {
            if (isResponseAvaliable()) {
                return true;
            }
        }
        LOG.debug("Waiting for response timeout");
        return false;
    }

    /**
     * Write the specified bytes to my output stream.
     *
     * @param data the data to be written
     * @throws HttpRecoverableException if a SocketException occurs
     * @throws IllegalStateException if not connected
     * @throws IOException if an I/O problem occurs
     * @see #write(byte[],int,int)
     */
    public void write(byte[] data)
        throws IOException, IllegalStateException, HttpRecoverableException {
        LOG.trace("enter HttpConnection.write(byte[])");
        this.write(data, 0, data.length);
    }

    /**
     * Write <i>length</i> bytes in <i>data</i> starting at
     * <i>offset</i> to my output stream.
     *
     * The general contract for
     * write(b, off, len) is that some of the bytes in the array b are written
     * to the output stream in order; element b[off] is the first byte written
     * and b[off+len-1] is the last byte written by this operation.
     *
     * @param data array containing the data to be written.
     * @param offset the start offset in the data.
     * @param length the number of bytes to write.
     * @throws HttpRecoverableException if a SocketException occurs
     * @throws IllegalStateException if not connected
     * @throws IOException if an I/O problem occurs
     */
    public void write(byte[] data, int offset, int length)
        throws IOException, IllegalStateException, HttpRecoverableException {
        LOG.trace("enter HttpConnection.write(byte[], int, int)");

        if (offset + length > data.length) {
            throw new HttpRecoverableException( "Unable to write:"
                    + " offset=" + offset + " length=" + length
                    + " data.length=" + data.length);
        } else if (data.length <= 0) {
            throw new HttpRecoverableException(
                "Unable to write:" + " data.length=" + data.length);
        }

        assertOpen();

        if (WIRE_LOG.isDebugEnabled()) {
            String dataString = new String(data, offset, length, "ISO-8859-1");
            WIRE_LOG.debug(">> \"" + dataString + "\" [\\r\\n]");
        }
        try {
            outputStream.write(data, offset, length);
        } catch (SocketException se) {
            LOG.debug(
                "HttpConnection: Socket exception while writing data",
                se);
            throw new HttpRecoverableException(se.toString());
        } catch (IOException ioe) {
            LOG.debug("HttpConnection: Exception while writing data", ioe);
            throw ioe;
        }
    }

    /**
     * Write the specified bytes, followed by <tt>"\r\n".getBytes()</tt> to my
     * output stream.
     *
     * @param data the bytes to be written
     * @throws HttpRecoverableException when socket exceptions occur writing data
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     */
    public void writeLine(byte[] data)
        throws IOException, IllegalStateException, HttpRecoverableException {
        LOG.trace("enter HttpConnection.writeLine(byte[])");

        assertOpen();
        if (WIRE_LOG.isDebugEnabled() && (data.length > 0)) {
            String dataString = HttpConstants.getContentString(data);
            WIRE_LOG.debug(">> \"" + dataString.trim() + "\" [\\r\\n]");
        }
        try {
            outputStream.write(data);
            writeLine();
        } catch (SocketException se) {
            LOG.info("SocketException while writing data to output", se);
            throw new HttpRecoverableException(se.toString());
        } catch (IOException ioe) {
            LOG.info("IOException while writing data to output", ioe);
            throw ioe;
        }
    }

    /**
     * Write <tt>"\r\n".getBytes()</tt> to my output stream.
     *
     * @throws HttpRecoverableException when socket exceptions occur writing
     *      data
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     */
    public void writeLine()
        throws IOException, IllegalStateException, HttpRecoverableException {
        LOG.trace("enter HttpConnection.writeLine()");

        WIRE_LOG.debug(">> [\\r\\n]");
        try {
            outputStream.write(CRLF);
        } catch (SocketException se) {
            LOG.warn("HttpConnection: Socket exception while writing data", se);
            throw new HttpRecoverableException(se.toString());
        } catch (IOException ioe) {
            LOG.warn("HttpConnection: IO exception while writing data", ioe);
            throw ioe;
        }
    }

    /**
     * Write the specified String (as bytes) to my output stream.
     *
     * @param data the string to be written
     * @throws HttpRecoverableException when socket exceptions occur writing
     *      data
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     */
    public void print(String data)
        throws IOException, IllegalStateException, HttpRecoverableException {
        LOG.trace("enter HttpConnection.print(String)");
        write(HttpConstants.getBytes(data));
    }

    /**
     * Write the specified String (as bytes), followed by
     * <tt>"\r\n".getBytes()</tt> to my output stream.
     *
     * @param data the data to be written
     * @throws HttpRecoverableException when socket exceptions occur writing
     *      data
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     */
    public void printLine(String data)
        throws IOException, IllegalStateException, HttpRecoverableException {
        LOG.trace("enter HttpConnection.printLine(String)");
        writeLine(HttpConstants.getBytes(data));
    }

    /**
     * Write <tt>"\r\n".getBytes()</tt> to my output stream.
     *
     * @throws HttpRecoverableException when socket exceptions occur writing
     *      data
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     */
    public void printLine()
        throws IOException, IllegalStateException, HttpRecoverableException {
        LOG.trace("enter HttpConnection.printLine()");
        writeLine();
    }

    /**
     * Read up to <tt>"\r\n"</tt> from my (unchunked) input stream.
     * If the stream ends before the line terminator is found,
     * the last part of the string will still be returned.
     * '\r' and '\n' are allowed to appear individually in the stream.
     *
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     * @return a line from the response
     */
    public String readLine() throws IOException, IllegalStateException {
        LOG.trace("enter HttpConnection.readLine()");

        assertOpen();
        return HttpParser.readLine(inputStream);
    }

    /**
     * Shutdown my {@link Socket}'s output, via {@link Socket#shutdownOutput}.
     */
    public void shutdownOutput() {
        LOG.trace("enter HttpConnection.shutdownOutput()");

        try {
            // Socket.shutdownOutput is a JDK 1.3
            // method. We'll use reflection in case
            // we're running in an older VM
            Class[] paramsClasses = new Class[0];
            Method shutdownOutput =
                socket.getClass().getMethod("shutdownOutput", paramsClasses);
            Object[] params = new Object[0];
            shutdownOutput.invoke(socket, params);
        } catch (Exception ex) {
            LOG.debug("Unexpected Exception caught", ex);
            // Ignore, and hope everything goes right
        }
        // close output stream?
    }

    /**
     * Close my socket and streams.
     */
    public void close() {
        LOG.trace("enter HttpConnection.close()");
        closeSocketAndStreams();
    }

    /**
     * Returns the httpConnectionManager.
     * @return HttpConnectionManager
     */
    public HttpConnectionManager getHttpConnectionManager() {
        return httpConnectionManager;
    }

    /**
     * Sets the httpConnectionManager.
     * @param httpConnectionManager The httpConnectionManager to set
     */
    public void setHttpConnectionManager(HttpConnectionManager httpConnectionManager) {
        this.httpConnectionManager = httpConnectionManager;
    }

    /**
     * Release the connection.
     */
    public void releaseConnection() {
        LOG.trace("enter HttpConnection.releaseConnection()");
        if (httpConnectionManager != null) {
            httpConnectionManager.releaseConnection(this);
        }
    }

    // ------------------------------------------------------ Protected Methods

    /**
     * Close everything out.
     */
    protected void closeSocketAndStreams() {
        LOG.trace("enter HttpConnection.closeSockedAndStreams()");

        // no longer care about previous responses...
        lastResponseInputStream = null;

        if (null != inputStream) {
            try {
                inputStream.close();
            } catch (Exception ex) {
                LOG.debug("Exception caught when closing input", ex);
                // ignored
            }
            inputStream = null;
        }

        if (null != outputStream) {
            try {
                outputStream.close();
            } catch (Exception ex) {
                LOG.debug("Exception caught when closing output", ex);
                // ignored
            }
            outputStream = null;
        }

        if (null != socket) {
            try {
                socket.close();
            } catch (Exception ex) {
                LOG.debug("Exception caught when closing socket", ex);
                // ignored
            }
            socket = null;
        }
        isOpen = false;
        tunnelEstablished = false;
        usingSecureSocket = false;
    }

    /**
     * Throw an {@link IllegalStateException} if I am connected.
     *
     * @throws IllegalStateException if connected
     */
    protected void assertNotOpen() throws IllegalStateException {
        if (isOpen) {
            throw new IllegalStateException("Connection is open");
        }
    }

    /**
     * Throw an {@link IllegalStateException} if I am not connected.
     *
     * @throws IllegalStateException if not connected
     */
    protected void assertOpen() throws IllegalStateException {
        if (!isOpen) {
            throw new IllegalStateException("Connection is not open");
        }
    }

    // -- Timeout Exception
    /**
     * Signals that a timeout occured while opening the socket.
     */
    public class ConnectionTimeoutException extends IOException {
        /** Create an instance */
        public ConnectionTimeoutException() {
        }
    }


    /**
     * Helper class for wrapping socket based tasks.
     */
    private abstract class SocketTask implements Runnable {

        /** The socket */
        private Socket socket;
        /** The exception */
        private IOException exception;

        /**
         * Set the socket.
         * @param newSocket The new socket.
         */
        protected void setSocket(final Socket newSocket) {
            socket = newSocket;
        }

        /**
         * Return the socket.
         * @return Socket The socket.
         */
        protected Socket getSocket() {
            return socket;
        }
        /**
         * Perform the logic.
         * @throws IOException If an IO problem occurs
         */
        public abstract void doit() throws IOException;

        /** Execute the logic in this object and keep track of any exceptions. */
        public void run() {
            try {
                doit();
            } catch (IOException e) {
                exception = e;
            }
        }
    }

    // ------------------------------------------------------- Static Variable

    /** <tt>"\r\n"</tt>, as bytes. */
    private static final byte[] CRLF = new byte[] {(byte) 13, (byte) 10};

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(HttpConnection.class);
    
    /** Log for any wire messages. */
    private static final Log WIRE_LOG = LogFactory.getLog("httpclient.wire");
    
    // ----------------------------------------------------- Instance Variables    
    /** My host. */
    private String hostName = null;
    
    /** My port. */
    private int portNumber = -1;
    
    /** My proxy host. */
    private String proxyHostName = null;
    
    /** My proxy port. */
    private int proxyPortNumber = -1;
    
    /** My client Socket. */
    private Socket socket = null;
    
    /** My InputStream. */
    private InputStream inputStream = null;
    
    /** My OutputStream. */
    private OutputStream outputStream = null;
    
    /** An {@link InputStream} for the response to an individual request. */
    private InputStream lastResponseInputStream = null;
    
    /** Whether or not I am connected. */
    private boolean isOpen = false;
    
    /** the protocol being used */
    private Protocol protocolInUse;
    
    /** SO_TIMEOUT socket value */
    private int soTimeout = 0;
    
    /** TCP_NODELAY socket value */
    private boolean soNodelay = true;
    
    /** Whether or not the _socket is a secure one. Note the difference to _ssl */
    private boolean usingSecureSocket = false;
    
    /** Whether I am tunneling a proxy or not */
    private boolean tunnelEstablished = false;
    
    /** Timeout until connection established (Socket created). 0 means no timeout. */
    private int connectTimeout = 0;
    
    /** the connection manager that created this connection or null */
    private HttpConnectionManager httpConnectionManager;
}
