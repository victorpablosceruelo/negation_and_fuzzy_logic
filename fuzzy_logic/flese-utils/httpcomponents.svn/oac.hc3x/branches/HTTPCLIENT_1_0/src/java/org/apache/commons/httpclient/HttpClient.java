/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpClient.java,v 1.34 2001/09/01 21:40:45 remm Exp $
 * $Revision: 1.34 $
 * $Date: 2001-09-01 23:40:46 +0200 (Sat, 01 Sep 2001) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Tomcat", and "Apache Software
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
import java.util.*;
import java.security.Principal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.net.Socket;
import java.net.UnknownHostException;
import java.net.URL;
import java.lang.reflect.Method;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

/**
 * HTTP client main class.
 * <p>
 * HTTPS URLs are now supported.  To use HTTPS, you'll
 * need an implementation of the
 * <a href="http://java.sun.com/products/jsse/">Java Secure Sockets Extension</a>
 * installed and configured.  Make sure that you've enabled
 * the appropriate provider with your JRE (e.g., set
 * <br>
 * <tt>security.provider.<i>&lt;n&gt;</i>=com.sun.net.ssl.internal.ssl.Provider</tt>
 * <br>
 * in your JRE's <tt>java.security</tt> file) and with the
 * VM that is running the HttpClient (e.g., set the
 * system property <tt>java.protocol.handler.pkgs</tt>
 * to include your provider, such as 
 * <tt>com.sun.net.ssl.internal.www.protocol</tt>
 * for the reference implementation of JSSE).
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author <a href="mailto:rwaldhoff@apache.org">Rodney Waldhoff</a>
 */
public class HttpClient {


    // -------------------------------------------------------------- Constants


    /**
     * HTTP Date format pattern (RFC 2068, 822, 1123).
     */
    public static final String DATE_FORMAT = "EEE, d MMM yyyy kk:mm:ss z";


    /**
     * User Agent.
     */
    public static final Header USER_AGENT =
        new Header("User-Agent", "Jakarta HTTP Client/1.0");


    /**
     * Date formatter.
     */
    protected static final DateFormat formatter =
        new SimpleDateFormat(DATE_FORMAT);


    // ----------------------------------------------------------- Constructors


    /**
     * Preferred constructor for HttpClient.  Call
     * {@link #startSession(URL)} or
     * {@link #startSession(String, int)} to begin a
     * session.
     *
     * <b>Note:</b> The HttpClient constructors used to
     * implicitly start a session.  That behavior has been
     * removed.
     */
    public HttpClient() {
        //startSession();
    }


    /**
     *
     * <b>Note:</b> The HttpClient constructors used to
     * implicitly start a session.  That behavior has been
     * removed.
     * @param user
     * @param password
     * @deprecated Use a startSession method that takes credentials
     *             instead
     */
    public HttpClient(String user, String password) {
        setCredentials(new Credentials(user, password));
    }


    /**
     * <b>Note:</b> The HttpClient constructor used to
     * implicitly start a session.  That behavior has been
     * removed.
     *
     * @param url
     * @deprecated This constructor was considered misleading in
     *             conjunction with the {@link #startSession()}
     */
    public HttpClient(URL url) {
        //startSession(url);
    }


    /**
     *
     * @param url
     * @param user
     * @param password
     * @deprecated This constructor was considered misleading in
     *             conjunction with the {@link #startSession()}
     */
    public HttpClient(URL url, String user, String password) {
        this(url);
        /*
         * when this constructor is finally removed,
         * the endSession method should be altered
         * to null out the credentials
         */
        setCredentials(new Credentials(user, password));
    }




    // ----------------------------------------------------- Instance Variables


    /**
     * Client Socket in use.
     */
    protected Socket socket;


    /**
     * Session state.
     */
    protected State state;


    /**
     * Credentials to use.
     */
    protected Credentials credentials;


    /**
     * Socket input stream.
     */
    protected InputStream input;


    /**
     * Socket output stream.
     */
    protected OutputStream output;


    /**
     * The host name specified when the startSession(host, port) method was
     * called.
     */
    protected String sessionHost = "";


    /**
     * Proxy port number.
     */
    protected int proxyPort = -1;


    /**
     * Proxy hostname.
     */
    protected String proxyHost = null;


    /**
     * Port number.
     */
    protected int sessionPort = -1;


    /**
     * HTTP/1.1 flag.
     */
    protected boolean http11 = false;

    /**
     * HTTPS flag.
     */
    protected boolean https = false;

    /**
     * Stream interceptors.
     */
    protected StreamInterceptor streamInterceptor = null;


    /**
     * Connection interceptors.
     */
    protected ConnectionInterceptor connectionInterceptor = null;


    /**
     * Debug level.
     */
    protected int debug = 0;


    // ------------------------------------------------------------- Properties


    /**
     * Set the socket to use.
     */
    public void setSocket(Socket socket) {
        this.socket = socket;
    }


    /**
     * Set the credentials to use.
     */
    public void setCredentials(Credentials credentials) {
        this.credentials = credentials;
    }


    /**
     * Get the credentials used.
     */
    public Credentials getCredentials() {
        return credentials;
    }


    /**
     * Set debug level.
     */
    public void setDebug(int debug) {
        this.debug = debug;
    }


    /**
     * Get debug level.
     */
    public int getDebug() {
        return debug;
    }


    /**
     * Get the session host.
     */
    public String getHost() {
        return sessionHost;
    }


    /**
     * Get the session port.
     */
    public int getPort() {
        return sessionPort;
    }


    /**
     * Get the state for lock information.
     */
    public State getState() {
        return state;
    }


    /**
     * Set the client state.
     */
    public void setState(State state) {
        this.state = state;
    }


    /**
     * Get the username.
     */
    public String getUserName() {
        return (credentials != null) ? credentials.getUserName() : null;
    }


    /**
     * Get the password.
     */
    public String getPassword() {
        return (credentials != null) ? credentials.getPassword() : null;
    }


    /**
     * Set stream interceptor.
     */
    public void setStreamInterceptor
        (StreamInterceptor streamInterceptor) {
        this.streamInterceptor = streamInterceptor;
    }


    /**
     * Get stream interceptor.
     */
    StreamInterceptor getStreamInterceptor() {
        return streamInterceptor;
    }


    /**
     * Set connection interceptor.
     */
    public void setConnectionInterceptor
        (ConnectionInterceptor connectionInterceptor) {
        this.connectionInterceptor = connectionInterceptor;
    }


    /**
     * Get connection interceptor.
     */
    ConnectionInterceptor getConnectionInterceptor() {
        return connectionInterceptor;
    }


    // --------------------------------------------------------- Public Methods


    /**
     * Execute a DAV method.
     *
     * @param method WebDAV method to execute
     */
    public synchronized void executeMethod(HttpMethod method)
        throws IOException, HttpException {

        int retries = 0;

        Hashtable responseHeaders = null;

        boolean methodProcessed = false;
        boolean sentRequestBody = false;

        openConnection();

        while ((retries < 5) && (!methodProcessed)) {

            try {

                responseHeaders = null;
                sentRequestBody = false;

                // Send the request header except if the
                byte[] query = sendRequestHeader(method);

                if ((!http11) || (!method.needExpectation())) {
                    sendRequestBody(method, query);
                    sentRequestBody = true;
                }

                if (connectionInterceptor != null) {
                    connectionInterceptor.sentRequest();
                }

                boolean closeOutput = needToCloseOutput(method);
                if (closeOutput) {
                    try {
                        Class[] paramsClasses = new Class[0];
                        Method shutdownOutput = socket.getClass().getMethod
                            ("shutdownOutput", paramsClasses);
                        Object[] params = new Object[0];
                        shutdownOutput.invoke(socket, params);
                    } catch (Exception e) {
                        // Ignore, and hope everything goes right
                    }
                }

                // Parsing response

                // Parse status line
                String statusLine = readLine(input);
                if (statusLine == null)
                    throw new IOException("Couldn't parse status line");
                parseStatusLine(statusLine, method);

                // Parse headers
                responseHeaders = parseHeaders(input);

                while (method.getStatusCode() < 200) {

                    if (connectionInterceptor != null) {
                        connectionInterceptor.info
                            (method.getStatusCode(), responseHeaders);
                    }

                    if (method.getStatusCode() == HttpStatus.SC_CONTINUE) {
                        if (!sentRequestBody) {
                            if (connectionInterceptor != null) {
                                connectionInterceptor.receivedExpectation();
                            }
                            sendRequestBody(method, query);
                            sentRequestBody = true;
                        }
                    }

                    statusLine = readLine(input);
                    if (statusLine == null)
                        throw new IOException("Couldn't parse status line");
                    parseStatusLine(statusLine, method);

                    // Parse headers
                    responseHeaders = parseHeaders(input);

                }

                if (connectionInterceptor != null) {
                    connectionInterceptor.receivedResponse();
                }

                // Retrieve the authenticate challenge, if any
                // (needed in case of a digest challenge, for which the
                // header is not constant)
                Header authenticateChallenge =
                    (Header) responseHeaders.get("www-authenticate");
                if (authenticateChallenge != null) {
                    state.setAuthenticateToken
                        (authenticateChallenge.getValue());
                    if (connectionInterceptor != null) {
                        connectionInterceptor.requiredAuthentication();
                    }
                }

                if ((method.getStatusCode()
                     == HttpStatus.SC_MOVED_TEMPORARILY)
                    || (method.getStatusCode()
                        == HttpStatus.SC_MOVED_PERMANENTLY)) {
                    if (method.followRedirects()) {
                        // Retrieve the location header
                        // NOTE : Redirects across servers are not
                        // supported yet (and perhaps will never be for
                        // security reasons)
                        Header location =
                            (Header) responseHeaders.get("location");
                        if (location != null) {
                            String absolutePath = location.getValue();
                            if (absolutePath.startsWith("http://")) {
                                absolutePath = absolutePath.substring(7);
                            }
                            int slash = absolutePath.indexOf('/');
                            if (slash != -1) {
                                absolutePath = absolutePath.substring(slash);
                            }
                            if (absolutePath.equals("")) {
                                absolutePath = "/";
                            }
                            method.setPath(absolutePath);
                        }
                    } else {
                        methodProcessed = true;
                    }
                }

                if ( (method.getStatusCode() != HttpStatus.SC_UNAUTHORIZED)
                     && (method.getStatusCode()
                         != HttpStatus.SC_MOVED_TEMPORARILY)
                     && (method.getStatusCode()
                         != HttpStatus.SC_MOVED_PERMANENTLY)
                     && (method.getStatusCode()
                         != HttpStatus.SC_CONTINUE)) {
                    methodProcessed = true;
                } else {
                    if (!methodProcessed) {

                        if (connectionInterceptor != null) {
                            connectionInterceptor.retry
                                (method.getStatusCode());
                        }

                        // Consume bytes returned (if any)
                        method.processResponseHeaders(responseHeaders);
                        ResponseInputStream responseInputStream =
                            new ResponseInputStream(input, method, 
                                                    responseHeaders);
                        // FIXME : Really set the interceptors here ?
                        // The content is meant to be discarded
                        //responseInputStream.setInterceptor
                        //    (streamInterceptor);
                        responseInputStream.close();
                        if (closeOutput ||
                            needToCloseConnection(method, responseHeaders)) {
                            // Disconnect and reconnect if needed
                            closeConnection();
                            openConnection();
                        }
                    }
                }

            } catch (IOException e) {
                if (connectionInterceptor != null) {
                    connectionInterceptor.error(method.getStatusCode(), e);
                }
                if (debug > 1)
                    e.printStackTrace();
                // If something goes wrong, disconnect, then reconnect
                try {
                    closeConnection();
                } catch (IOException ex) {
                    // Silent catch
                }
                openConnection();
            } catch (HttpException e) {
                if (connectionInterceptor != null) {
                    connectionInterceptor.error(e.getStatusCode(), e);
                }
                // During communication, save the status code.
                if (e.getStatusCode() > 0)
                    method.setStatusCode(e.getStatusCode());
                if (debug > 1)
                    System.err.println("HTTP Exception: " + e.getMessage() +
                                       ", Status Code: " + e.getStatusCode());
                // If something goes wrong, disconnect, then reconnect
                try {
                    closeConnection();
                } catch (IOException ex) {
                    // Silent catch
                }
                if (e.getStatusCode() == HttpException.NO_CREDENTIALS_GIVEN)
                    throw new HttpException(HttpStatus.SC_UNAUTHORIZED);
                openConnection();
            }

            retries++;

        }

        if (retries == 5) {
            throw new HttpException
                ("Unable to process request", method.getStatusCode());
        }

        method.processResponseHeaders(responseHeaders);
        method.setUsed();
        // Parse response
        ResponseInputStream responseInputStream =
            new ResponseInputStream(input, method, responseHeaders);
        responseInputStream.setInterceptor(streamInterceptor);

        method.parseResponse(responseInputStream);

        responseInputStream.close();

        if (needToCloseConnection(method, responseHeaders)) {
            closeConnection();
        }

    }


    /**
     * Start a session.
     *
     * @deprecated This method opens a connection to
     *             localhost:80.  It will be removed in a future release
     *             to make usage more clear.
     */
    public void startSession() {

        if (state == null)
            state = new State();
        this.sessionHost = "localhost";
        this.sessionPort = 80;
        this.https = false;

    }


    /**
     * Start a session.
     */
    public void startSession(String host, int port) {
        startSession(host,port,false);
    }


    /**
     * Start a session.
     */
    public void startSession(String host, int port, boolean https) {
        if (debug > 0)
            System.out.println("Start session : Host:" + host
                               + " Port:" + port
                               + " HTTPS:" + https);

        if (state == null)
            state = new State();
        this.sessionHost = host;
        this.sessionPort = port;
        this.https = https;

    }


    /**
     * Start a session.
     */
    public void startSession(String host, int port, Credentials creds) {
        startSession(host,port,creds,false);
    }


    /**
     * Start a session.
     */
    public void startSession(String host, int port, Credentials creds, 
                             boolean https) {
        if (debug > 0)
            System.out.println("Start session : Host:" + host
                                + " Port:" + port
                                + " Credentials:" + creds
                                + " HTTPS:" + https);
        setCredentials(creds);
        if (state == null)
            state = new State();
        this.sessionHost = host;
        this.sessionPort = port;
        this.https = https;

    }


    /**
     * Start a session.  When starting a session, only the
     * protocol, hostname and port are considered.  The path must still
     * be set when constructing an HttpMethod (GetMethod,
     * PutMethod, etc.)
     *
     * @param url    URL object containing the target hostname and port
     */
    public void startSession(URL url) {
      if("https".equalsIgnoreCase(url.getProtocol())) {
        startSession(url.getHost(), url.getPort() == -1 ? 443 
                     : url.getPort(),true);
      } else if("http".equalsIgnoreCase(url.getProtocol())) {
        startSession(url.getHost(), url.getPort() == -1 ? 80 
                     : url.getPort(),false);
      } else {
          throw new IllegalArgumentException("Protocol " + url.getProtocol() 
                                             + " not supported in URL " + url);
      }
    }


    /**
     * Start a session.  When starting a session, only the
     * hostname and port are considered.  The path must still
     * be set when constructing an HttpMethod (GetMethod,
     * PutMethod, etc.)
     *
     * @param url    URL object containing the target hostname and port
     */
    public void startSession(URL url, Credentials creds) {
      setCredentials(creds);
      startSession(url.getHost(), url.getPort() == -1 ? 80 : url.getPort());
    }


    /**
     * Start a session with a proxy server.
     */
    public void startSession(String host, int port, 
                             String proxyhost, int proxyport) {
        this.proxyHost = proxyhost;
        this.proxyPort = proxyport;
        startSession(host, port);
    }


    /**
     * End a session.
     */
    public void endSession()
        throws IOException {

        if (debug > 0)
            System.out.println("End session");

        closeConnection();

        state = null;
        this.sessionHost = "";
        this.sessionPort = -1;
        this.https = false;

    }


    // ------------------------------------------------------ Protected Methods


    protected void openConnection() throws IOException, UnknownHostException {

        try {
            if (socket == null) {
                if (proxyHost == null || proxyPort < 0) {
                    if (debug > 0)
                        System.out.println
                            ("Reopen connection : Host:"
                             + sessionHost + " Port:" + sessionPort);
                    if(https) {
                        socket = SSLSocketFactory.getDefault().createSocket
                            (this.sessionHost, this.sessionPort);
                    } else {
                        socket = new Socket
                            (this.sessionHost, this.sessionPort);
                    }
                } else {
                    if (debug > 0)
                        System.out.println("Reopen connection : Host:"
                                           + proxyHost + " Port:" + proxyPort);
                    socket = new Socket(this.proxyHost, this.proxyPort);
                }
            }
            input = socket.getInputStream();
            output = socket.getOutputStream();
        } catch (IOException e) {
            if (connectionInterceptor != null) {
                connectionInterceptor.error(-1, e);
            }

            // Connection is probably half closed
            // Closing the connection and trying again
            if (socket != null) {
                socket.close();
                socket = null;
            }

            throw e;
        }

        if (connectionInterceptor != null) {
            connectionInterceptor.connect();
        }

    }


    protected void closeConnection() throws IOException {

        if (debug > 0)
            System.out.println("Closing connection");

        // Close socket
        if (input != null)
            input.close();
        input = null;
        if (output != null)
            output.close();
        output = null;
        if (socket != null)
            socket.close();
        socket = null;

        try {
            Thread.sleep(1);
        } catch (Exception ex) {
        }

        if (connectionInterceptor != null) {
            connectionInterceptor.disconnect();
        }

    }


    /**
     * Send a WebDAV request.
     *
     * @param method WebDAV method to execute
     */
    protected byte[] sendRequestHeader(HttpMethod method)
        throws IOException, HttpException {

        if (method.hasBeenUsed())
            throw new HttpException("Method has already been used");

        if (!method.validate())
            throw new HttpException("Invalid method");

        method.setState(state);

        String requestLine;
        if (proxyHost == null || proxyPort < 0)
            requestLine = method.generateRequestLine();
        else
            requestLine = method.generateRequestLine(getHost(), getPort());

        if (debug > 0) {
            System.out.println();
        }

        String hostName = sessionHost;
        if (sessionPort != 80)
            hostName = hostName + ":" + sessionPort;

        method.generateHeaders(hostName, state);

        Enumeration headersList = method.getHeaders();

        // Sending request line
        if (debug > 1)
            System.out.print(requestLine);
        output.write(requestLine.getBytes());

        // Sending headers
        byte[] query = null;
        if (!method.isStreamedQuery()) {
            String queryStr = method.generateQuery();
            if (queryStr == null)
                queryStr = new String();
            query = queryStr.getBytes("UTF8");
            if (method.needContentLength()) {
                if (debug > 1)
                    System.out.print("Content-Length: "
                                     + query.length + "\r\n");
                output.write(("Content-Length: "
                              + query.length + "\r\n").getBytes());
            }
        } else {
            // Chunking
            if ((http11) && (method.getHeader("Content-Length") == null)) {
                if (debug > 1)
                    System.out.print("Transfer-Encoding: chunked\r\n");
                output.write(("Transfer-Encoding: chunked\r\n").getBytes());
            }
        }

        if (state.getAuthenticateToken() != null) {

            String challengeResponse = Authenticator.challengeResponse
                (state, credentials);
            if (challengeResponse != null) {
                if (debug > 1)
                    System.out.print("Authorization: "
                                     + challengeResponse + "\r\n");
                output.write(("Authorization: "
                              + challengeResponse + "\r\n").getBytes());
                if (connectionInterceptor != null) {
                    connectionInterceptor.authenticate();
                }
            }

        }

        // Send expectation header
        if (method.needExpectation()) {
            output.write(("Expect: 100-continue\r\n").getBytes());
        }

        // Writing HTTP headers

        while (headersList.hasMoreElements()) {
            Header header = (Header) headersList.nextElement();
            if (debug > 1)
                System.out.print(header.toString());
            output.write(header.toString().getBytes());
        }

        if (debug > 1)
            System.out.print("\r\n");
        output.write("\r\n".getBytes());

        return query;

    }


    /**
     * Send a WebDAV request.
     *
     * @param method WebDAV method to execute
     */
    protected void sendRequestBody(HttpMethod method, byte[] query)
        throws IOException, HttpException {

        // Writing request body

        RequestOutputStream requestOutputStream =
            new RequestOutputStream(output, method);
        requestOutputStream.setInterceptor(streamInterceptor);

        if (method.isStreamedQuery()) {
            if ((http11) && (method.getHeader("Content-Length") == null)) {
                requestOutputStream.setUseChunking(true);
            }
            method.streamQuery(requestOutputStream);
        } else {
            if (debug > 10) {
                System.out.println(new String(query, "UTF8"));
                System.out.println();
            }
            requestOutputStream.write(query);
        }

        // Closing wrapped output stream
        requestOutputStream.close();

    }


    /**
     * Reads the input stream, one line at a time. Reads bytes into an array,
     * until it reads a certain number of bytes or reaches a newline character,
     * which it reads into the array as well.
     *
     * @param input Input stream on which the bytes are read
     * @return The line that was read, or <code>null</code> if end-of-file
     *  was encountered
     * @exception IOException   if an input or output exception has occurred
     */
    protected String readLine(InputStream input)
        throws IOException {

        StringBuffer sb = new StringBuffer();
        while (true) {
            int ch = input.read();
            if (ch < 0) {
                if (sb.length() == 0) {
                    return (null);
                } else {
                    break;
                }
            } else if (ch == '\r') {
                continue;
            } else if (ch == '\n') {
                break;
            }
            sb.append((char) ch);
        }
        return (sb.toString());

    }


    /**
     * Parse status line.
     *
     * @param statusLine String representing the HTTP status line
     * @param method Http method
     */
    protected void parseStatusLine(String statusLine, HttpMethod method)
        throws IOException, HttpException {

        while (statusLine != null && !statusLine.startsWith("HTTP/")) {
            statusLine = readLine(input);
        }
        if (debug > 0) {
            System.out.println();
            System.out.println(statusLine);
        }
        if (statusLine == null)
            throw new HttpException
                ("Error in parsing the response: " + statusLine);

        if ((!statusLine.startsWith("HTTP/1.1") &&
             !statusLine.startsWith("HTTP/1.0")))
            throw new HttpException
                ("Incorrect server protocol :" + statusLine);

        http11 = statusLine.startsWith("HTTP/1.1");

        int statusCode = -1;

        int at = statusLine.indexOf(" ");
        if (at < 0)
            throw new HttpException
                ("Error in parsing the response: " + statusLine);

        int to = statusLine.indexOf(" ", at + 1);
        if (to < 0)
            to = statusLine.length();

        try {
            statusCode = Integer.parseInt(statusLine.substring(at + 1, to));
        } catch (NumberFormatException e) {
            throw new HttpException("Status not specified: " + statusLine);
        }

        method.setStatusCode(statusCode);

        String statusText = null;
        try {
            if (to < statusLine.length())
                statusText = statusLine.substring(to + 1);
        } catch (StringIndexOutOfBoundsException e) {
            throw new HttpException
                ("Status not specified: " + statusLine);
        }

        if (statusText != null)
            method.setStatusText(statusText);
    }


    /**
     * Parse headers.
     *
     * @param input Input stream on which the bytes are read
     */
    protected Hashtable parseHeaders(InputStream input)
        throws IOException, HttpException {

        Hashtable result = new Hashtable();

        while (true) {

            // Read the next header line
            String line = readLine(input);
            if ((line == null) || (line.length() < 1))
            break;

            // Parse the header name and value
            int colon = line.indexOf(":");
            if (colon < 0)
            throw new HttpException("Incorrect headers");
            String name = line.substring(0, colon).trim();
            String match = name.toLowerCase();
            String value = line.substring(colon + 1).trim();
            Header header = new Header(name, value);
            result.put(match, header);

            if (debug > 0) {
                System.out.println(name + ": " + value);
            }

        }

        // should we set cookies?
        Header header = (Header) result.get("set-cookie2");

        // if the server doesn't support new cookies,
        // we'll use the old cookies
        if (header == null) {
            header = (Header) result.get("set-cookie");
        }

        if (header != null) {
            try {
                Cookie[] cookies = Cookie.parse(sessionHost, header);
                state.addCookies(cookies);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        if (debug > 0) {
            System.out.println();
        }

        return result;

    }


    /**
     * Return true if the connection should be closed after processing.
     */
    protected boolean needToCloseConnection(HttpMethod method,
                                            Hashtable responseHeaders) {
        if (!http11)
            return true;

        Header connectionHeader = (Header) responseHeaders.get("connection");

        if ((connectionHeader != null)
            && (connectionHeader.getValue().equals("close")))
            return true;

        if ((method.getStatusCode() < 200)
            || (method.getStatusCode() == HttpStatus.SC_NO_CONTENT)
            || (method.getStatusCode() == HttpStatus.SC_NOT_MODIFIED))
            return false;

        Header teHeader = (Header) responseHeaders.get("transfer-encoding");
        boolean chunk = (teHeader != null)
            && (teHeader.getValue().indexOf("chunked") != -1);
        Header contentLengthHeader =
            (Header) responseHeaders.get("content-length");
        int contentLength = -1;
        if (contentLengthHeader != null) {
            try {
                contentLength =
                    Integer.parseInt(contentLengthHeader.getValue());
            } catch (Exception e) {
            }
        }

        if ((method.needContentLength()) && (contentLength == -1)
            && (!chunk)) {
            // Non compliant server ???
            return true;
        }

        return false;
    }


    /**
     * Return true if the connection should be closed after sending the
     * request.
     */
    protected boolean needToCloseOutput(HttpMethod method) {
        if (!http11) {
            if ((method.isStreamedQuery()) 
                && (method.getHeader("Content-Length") == null)
                && (method.needContentLength())) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }


}
