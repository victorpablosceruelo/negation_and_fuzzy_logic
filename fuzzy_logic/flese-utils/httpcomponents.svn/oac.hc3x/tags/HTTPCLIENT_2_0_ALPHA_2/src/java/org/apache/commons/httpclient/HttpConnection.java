/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpConnection.java,v 1.36 2003/01/25 04:46:39 jsdever Exp $
 * $Revision: 1.36 $
 * $Date: 2003-01-25 05:46:39 +0100 (Sat, 25 Jan 2003) $
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
 * 4. The names "The Jakarta Project", "HttpClient", and "Apache Software
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
 * @author Jeff Dever
 * @version $Revision: 1.36 $ $Date: 2003-01-25 05:46:39 +0100 (Sat, 25 Jan 2003) $
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
    public HttpConnection(String proxyHost, int proxyPort, String host, int port) {
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
    public HttpConnection(String proxyHost, int proxyPort, String host,
    int port, boolean secure) {
        this( 
            proxyHost, 
            proxyPort, 
            host, 
            port, 
            Protocol.getProtocol( secure ? "https" : "http" )
        );
    }

    /**
     * Creates a new HttpConnection.
     * 
     * @param hostConfiguration the host/proxy/protocol to use
     */
    public HttpConnection( HostConfiguration hostConfiguration ) {
        this(
            hostConfiguration.getProxyHost(),
            hostConfiguration.getProxyPort(),
            hostConfiguration.getHost(),
            hostConfiguration.getPort(),
            hostConfiguration.getProtocol()        
        );
    }

    public HttpConnection(
        String proxyHost, 
        int proxyPort, 
        String host,
        int port, 
        Protocol protocol
    ) {

        if( log.isDebugEnabled() ){
            log.debug(
                "HttpConnectionManager.getConnection:  creating "
                + " connection for " + host + ":" + port + " via " + proxyHost
                + ":" + proxyPort + " using protocol: " + protocol
            );
        }
        
        if ( host == null ) {
            throw new IllegalArgumentException("host parameter is null");
        }        
        if ( protocol == null ) {
            throw new IllegalArgumentException( "protocol is null" );
        }
        
        _proxyHost = proxyHost;
        _proxyPort = proxyPort;
        _host = host;
        _port = protocol.resolvePort( port );
        _protocol = protocol;
        
    }

    // ------------------------------------------ Attribute Setters and Getters

    /**
     * Return my host.
     *
     * @return my host.
     */
    public String getHost() {
        return _host;
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
        _host = host;
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
        if (_port < 0) {
            return isSecure() ? 443 : 80;
        } else {
            return _port;
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
        _port = port;
    }

    /**
     * Return my proxy host.
     *
     * @return my proxy host.
     */
    public String getProxyHost() {
        return _proxyHost;
    }

    /**
     * Set the host I should proxy through.
     *
     * @param host the host I should proxy through.
     * @throws IllegalStateException if I am already connected
     */
    public void setProxyHost(String host) throws IllegalStateException {
       assertNotOpen();
       _proxyHost = host;
    }

    /**
     * Return my proxy port.
     *
     * @return my proxy port.
     */
    public int getProxyPort() {
        return _proxyPort;
    }

    /**
     * Set the port I should proxy through.
     *
     * @param port the host I should proxy through.
     * @throws IllegalStateException if I am already connected
     */
    public void setProxyPort(int port) throws IllegalStateException {
       assertNotOpen();
       _proxyPort = port;
    }

    /**
     * Return <tt>true</tt> if I will (or I am) connected over a
     * secure (HTTPS/SSL) protocol.
     *
     * @return <tt>true</tt> if I will (or I am) connected over a
     *         secure (HTTPS/SSL) protocol.
     */
    public boolean isSecure() {
        return _protocol.isSecure();
    }

    /**
     * Get the protocol.
     * @return HTTPS if secure, HTTP otherwise
     */
    public Protocol getProtocol() {
        return _protocol;
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
        _protocol = (
            secure 
            ? Protocol.getProtocol( "https" ) 
            : Protocol.getProtocol( "http" )
        );
    }

    /**
     * Sets the protocol used by this connection.
     * 
     * @param protocol
     */
    public void setProtocol( Protocol protocol ) {
        assertNotOpen();
        
        if ( protocol == null ) {
            throw new IllegalArgumentException( "protocol is null" );
        }
        
        _protocol = protocol;
        
    }

    /**
     * Return <tt>true</tt> if I am connected,
     * <tt>false</tt> otherwise.
     *
     * @return <tt>true</tt> if I am connected
     */
    public boolean isOpen() {
        return _open;
    }

    /**
     * Return <tt>true</tt> if I am (or I will be)
     * connected via a proxy, <tt>false</tt> otherwise.
     *
     * @return <tt>true</tt> if I am (or I will be)
     *         connected via a proxy, <tt>false</tt> otherwise.
     */
    public boolean isProxied() {
        return (!(null == _proxyHost || 0 >= _proxyPort));
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
        _lastResponseInput = inStream;
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
        return _lastResponseInput;
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
        log.debug("HttpConnection.setSoTimeout("+ timeout +")");
        _so_timeout = timeout;
        if(_socket != null){
            _socket.setSoTimeout(timeout);
        }
    }

    /**
     * Sets the connection timeout. This is the maximum time that may be spent
     * until a connection is established. The connection will fail after this
     * amount of time.
     * @param timeout The timeout in milliseconds. 0 means timeout is not used.
     */
    public void setConnectionTimeout(int timeout) {
        this.connect_timeout = timeout;
    }

    /**
     * Open this connection to the current host and port
     * (via a proxy if so configured).
     * The underlying socket is created from the {@link ProtocolSocketFactory}.
     *
     * @throws IOException when there are errors opening the connection
     */
    public void open() throws IOException {
        log.trace("enter HttpConnection.open()");

        assertNotOpen(); // ??? is this worth doing?
        try {
            if (null == _socket) {
                
                final String host = (null == _proxyHost) ? _host : _proxyHost;
                final int port = (null == _proxyHost) ? _port : _proxyPort;
                
                _usingSecureSocket = isSecure() && !isProxied();                
                
                final ProtocolSocketFactory socketFactory = (
                    isSecure() && !isProxied()
                    ? _protocol.getSocketFactory()
                    : new DefaultProtocolSocketFactory()
                );
                
                if (connect_timeout == 0) {
                    _socket = socketFactory.createSocket(host,port);
                } else {
                    SocketTask task = new SocketTask() {
                        public void doit() throws IOException {
                            s = socketFactory.createSocket(host,port);
                        }
                    };
                    TimeoutController.execute(task, connect_timeout);
                    _socket = task.s;
                    if (task.exception != null) throw task.exception;
                }
                
            }
            
            /*
            "Nagling has been broadly implemented across networks, 
            including the Internet, and is generally performed by default 
            - although it is sometimes considered to be undesirable in 
            highly interactive environments, such as some client/server 
            situations. In such cases, nagling may be turned off through 
            use of the TCP_NODELAY sockets option." */
            
            _socket.setTcpNoDelay(_so_nodelay);
            _socket.setSoTimeout(_so_timeout);
            _input = _socket.getInputStream();
            _output = _socket.getOutputStream();
            _open = true;
        } catch (IOException e) {
            // Connection wasn't opened properly
            // so close everything out
            closeSocketAndStreams();
            throw e;
        } catch (TimeoutController.TimeoutException e) {
            if (log.isWarnEnabled()) {
                log.warn("The host "+ _host +":"+ _port +" (or proxy "+
                 _proxyHost +":"+ _proxyPort +") did not accept the connection "+
                 "within timeout of "+ connect_timeout +" milliseconds");
            }
            throw new ConnectionTimeoutException();
        }
    }

    /**
     * Calling this method indicates that the proxy has successfully created
     * the tunnel to the host. The socket will be switched to the secure socket.
     * Subsequent communication is done via the secure socket. The method can only
     * be called once on a proxied secure connection.
     *
     * @throws IllegalStateException if connection is not secure and proxied or
     * if the socket is already secure.
     * @throws IOException if an error occured creating the secure socket
     */
    public void tunnelCreated()
    throws IllegalStateException, IOException {
        log.trace("enter HttpConnection.tunnelCreated()");

        if (!isSecure() || !isProxied()) {
            throw new IllegalStateException("Connection must be secure and proxied to use this feature");
        }
        
        if (_usingSecureSocket) {
            throw new IllegalStateException("Already using a secure socket");
        }

        SecureProtocolSocketFactory socketFactory = (SecureProtocolSocketFactory)
            _protocol.getSocketFactory();

        _socket = socketFactory.createSocket(_socket, _host, _port, true);
        _input = _socket.getInputStream();
        _output = _socket.getOutputStream();
        _usingSecureSocket = true;
        _tunnelEstablished = true;
        log.debug("Secure tunnel created");
    }


    /**
     * Indicates if the connection is completely transparent from end to end.
     *
     * @return true if conncetion is not proxied or tunneled through a transparent
     * proxy; false otherwise.
     */
    public boolean isTransparent() {
        return !isProxied() || _tunnelEstablished;
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
        log.trace("enter HttpConnection.getRequestOutputStream()");
        assertOpen();
        return _output;
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
        log.trace("enter HttpConnection.getRequestOutputStream(boolean)");

        assertOpen();
        if (useChunking) {
            return new ChunkedOutputStream(_output);
        } else {
            return _output;
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
        log.trace("enter HttpConnection.getResponseInputStream(HttpMethod)");
        return getResponseInputStream();
    }

    public InputStream getResponseInputStream()
    throws IOException, IllegalStateException {
        log.trace("enter HttpConnection.getResponseInputStream()");
        assertOpen();
        return _input;
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
        log.trace("enter HttpConnection.write(byte[])");
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
        log.trace("enter HttpConnection.write(byte[], int, int)");

        if (offset+length > data.length){
            throw new HttpRecoverableException("Unable to write:" +
                    " offset=" + offset +
                    " length=" + length +
                    " data.length=" + data.length);
        }else if (data.length <= 0){
            throw new HttpRecoverableException("Unable to write:" +
                    " data.length=" + data.length);
        }

        assertOpen();

        if(wireLog.isDebugEnabled()) {
            String data_str =  new String(data, offset, length, "ISO-8859-1");
            wireLog.debug(">> \"" + data_str + "\" [\\r\\n]" );
        }
        try {
            _output.write(data, offset, length);
        } catch(SocketException se){
            log.debug("HttpConnection: Socket exception while writing data", se);
            throw new HttpRecoverableException(se.toString());
        } catch(IOException ioe) {
            log.debug("HttpConnection: Exception while writing data", ioe);
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
        log.trace("enter HttpConnection.writeLine(byte[])");

        assertOpen();
        if(wireLog.isDebugEnabled() && (data.length > 0)) {
            String data_str =  HttpConstants.getContentString(data);
            wireLog.debug(">> \"" + data_str.trim() + "\" [\\r\\n]" );
        }
        try{
            _output.write(data);
            writeLine();
        } catch(SocketException se){
            log.info("SocketException while writing data to output", se);
            throw new HttpRecoverableException(se.toString());
        } catch(IOException ioe){
            log.info("IOException while writing data to output", ioe);
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
        log.trace("enter HttpConnection.writeLine()");

        wireLog.debug(">> [\\r\\n]");
        try{
            _output.write(CRLF);
        } catch(SocketException se){
            log.warn("HttpConnection: Socket exception while writing data", se);
            throw new HttpRecoverableException(se.toString());
        } catch(IOException ioe){
            log.warn("HttpConnection: IO exception while writing data", ioe);
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
        log.trace("enter HttpConnection.print(String)");
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
        log.trace("enter HttpConnection.printLine(String)");
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
        log.trace("enter HttpConnection.printLine()");
        writeLine();
    }

    /**
     * Read up to <tt>"\r\n"</tt> from my (unchunked) input stream.
     *
     * @throws IllegalStateException if I am not connected
     * @throws IOException if an I/O problem occurs
     * @return a line from the response
     */
    public String readLine()
    throws IOException, IllegalStateException {
        log.trace("enter HttpConnection.readLine()");

        assertOpen();
        StringBuffer buf = new StringBuffer();
        for(;;) {
            int ch = _input.read();
            if(ch < 0) {
                if(buf.length() == 0) {
                    return null;
                } else {
                    break;
                }
            } else if (ch == '\r') {
//                log.debug("HttpConnection.readLine() found \\r, continuing");
                continue;
            } else if (ch == '\n') {
//                log.debug("HttpConnection.readLine() found \\n, breaking");
                break;
            }
            buf.append((char)ch);
        }
        if(wireLog.isDebugEnabled() && buf.length() > 0) {
            wireLog.debug("<< \"" + buf.toString() + "\" [\\r\\n]");
        }
        return (buf.toString());
    }

    /**
     * Shutdown my {@link Socket}'s output, via {@link Socket#shutdownOutput}.
     */
    public void shutdownOutput() {
        log.trace("enter HttpConnection.shutdownOutput()");

        try {
            // Socket.shutdownOutput is a JDK 1.3
            // method. We'll use reflection in case
            // we're running in an older VM
            Class[] paramsClasses = new Class[0];
            Method shutdownOutput = _socket.getClass().getMethod
                ("shutdownOutput", paramsClasses);
            Object[] params = new Object[0];
            shutdownOutput.invoke(_socket, params);
        } catch (Exception ex) {
            log.debug("Unexpected Exception caught", ex);
            // Ignore, and hope everything goes right
        }
        // close output stream?
    }

    /**
     * Close my socket and streams.
     */
    public void close() {
        log.trace("enter HttpConnection.close()");
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
    
    public void releaseConnection() {
        log.trace("enter HttpConnection.releaseConnection()");
        if ( httpConnectionManager != null ) {
            httpConnectionManager.releaseConnection( this );   
        }   
    }

    // ------------------------------------------------------ Protected Methods


    /**
     * Close everything out.
     */
    protected void closeSocketAndStreams() {
        log.trace("enter HttpConnection.closeSockedAndStreams()");

        // no longer care about previous responses...
        _lastResponseInput = null;

        if (null != _input) {
            try {
                _input.close();
            } catch(Exception ex) {
            log.debug("Exception caught when closing input", ex);
                // ignored
            }
            _input = null;
        }

        if (null != _output) {
            try {
                _output.close();
            } catch(Exception ex) {
            log.debug("Exception caught when closing output", ex);
                // ignored
            }
            _output = null;
        }

        if (null != _socket) {
            try {
                _socket.close();
            } catch(Exception ex) {
            log.debug("Exception caught when closing socket", ex);
                // ignored
            }
            _socket = null;
        }
        _open = false;
        _tunnelEstablished = false;
        _usingSecureSocket = false;
    }

    /**
     * Throw an {@link IllegalStateException} if I am connected.
     *
     * @throws IllegalStateException if connected
     */
    protected void assertNotOpen()
    throws IllegalStateException {
        if(_open) {
            throw new IllegalStateException("Connection is open");
        }
    }

    /**
     * Throw an {@link IllegalStateException} if I am not connected.
     *
     * @throws IllegalStateException if not connected
     */
    protected void assertOpen()
    throws IllegalStateException {
        if(!_open) {
            throw new IllegalStateException("Connection is not open");
        }
    }

    // -- Timeout Exception
    /**
     * Signals that a timeout occured while opening the socket.
     */
    public class ConnectionTimeoutException extends IOException {
        public ConnectionTimeoutException() {
        }
    }

    // -- Helper
    private abstract class SocketTask implements Runnable {
        public Socket s;
        public IOException exception;

        public abstract void doit() throws IOException;

        public void run() {
            try {
                doit();
            } catch(IOException e) {
                exception = e;
            }
        }
    }
    
    // ------------------------------------------------------------- Attributes

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(HttpConnection.class);
    /** Log for any wire messages. */
    private static final Log wireLog = LogFactory.getLog("httpclient.wire");
    /** My host. */
    private String _host = null;
    /** My port. */
    private int _port = -1;
    /** My proxy host. */
    private String _proxyHost = null;
    /** My proxy port. */
    private int _proxyPort = -1;
    /** My client Socket. */
    private Socket _socket = null;
    /** My InputStream. */
    private InputStream _input = null;
    /** My OutputStream. */
    private OutputStream _output = null;
    /** An {@link InputStream} for the response to an individual request. */
    private InputStream _lastResponseInput = null;
    /** Whether or not I am connected. */
    private boolean _open = false;
    /** the protocol being used */
    private Protocol _protocol;
    /** <tt>"\r\n"</tt>, as bytes. */
    private static final byte[] CRLF = HttpConstants.getBytes("\r\n");
    /** SO_TIMEOUT socket value */
    private int _so_timeout = 0;
    /** TCP_NODELAY socket value */
    private boolean _so_nodelay = true;
    /** Whether or not the _socket is a secure one. Note the difference to _ssl */
    private boolean _usingSecureSocket = false;
    /** Whether I am tunneling a proxy or not */
    private boolean _tunnelEstablished = false;
    /** Timeout until connection established (Socket created). 0 means no timeout. */
    private int connect_timeout = 0;
    /** the connection manager that created this connection or null */
    private HttpConnectionManager httpConnectionManager;
}
