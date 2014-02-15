/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HttpClient.java,v 1.31.2.11 2001/10/01 16:58:02 rwaldhoff Exp $
 * $Revision: 1.31.2.11 $
 * $Date: 2001-10-01 18:58:05 +0200 (Mon, 01 Oct 2001) $
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
import java.util.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.net.Socket;
import java.net.URL;
import org.apache.commons.httpclient.log.*;

/**
 * <p>
 * An HTTP "user-agent", containing an {@link HttpState} and
 * one or more {@link HttpConnection}s, to which
 * {@link HttpMethod}s can be applied.
 * </p>
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Rodney Waldhoff
 * @version $Revision: 1.31.2.11 $ $Date: 2001-10-01 18:58:05 +0200 (Mon, 01 Oct 2001) $
 */
public class HttpClient {


    // -------------------------------------------------------------- Constants

    /** <tt>org.apache.commons.httpclient.HttpClient</tt> log. */
    static private final Log log = LogSource.getInstance("org.apache.commons.httpclient.HttpClient");

    // ----------------------------------------------------------- Constructors

    /**
     * Constructor.
     */
    public HttpClient() {
    }

    // ----------------------------------------------------- Instance Variables

    /**
     * My {@link HttpConnection connection}.
     */
    protected HttpConnection connection = null;

    /**
     * Session state.
     */
    protected HttpState state;

    // ------------------------------------------------------------- Properties

    /**
     * Set debug level.
     * @see org.apache.commons.httpclient.log
     */
    public void setDebug(int debug) {
        log.setLevel(debug);
    }

    /**
     * Get debug level.
     * @see org.apache.commons.httpclient.log
     */
    public int getDebug() {
        return log.getLevel();
    }

    /**
     * Get my state.
     */
    public HttpState getState() {
        if(null == state) {
            state = new HttpState();
        }
        return state;
    }

    /**
     * Set my state.
     */
    public void setHttpState(HttpState state) {
        this.state = state;
    }

    // --------------------------------------------------------- Public Methods

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
        if(log.isDebugEnabled()) {
            log.debug("HttpClient.startSession(String,int,boolean): Host:" + host + " Port:" + port + " HTTPS:" + https);
        }
        if(null == state) {
            state = new HttpState();
        }
        connection = new HttpConnection(host,port,https);
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
        log.debug("HttpClient.startSession(String,int,Credentials,boolean): Host:" +
                  host + " Port:" + port + " Credentials:" + creds +
                  " HTTPS:" + https);
        if(null == state) {
            state = new HttpState();
        }
        state.setCredentials(null,creds);
        connection = new HttpConnection(host,port,https);
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
        if(null == state) {
            state = new HttpState();
        }
        state.setCredentials(null,creds);
        startSession(url);
    }


    /**
     * Start a session with a proxy server.
     */
    public void startSession(String host, int port,
                             String proxyhost, int proxyport) {
        if(null == state) {
            state = new HttpState();
        }
        connection = new HttpConnection(proxyhost,proxyport,host,port,false);
    }

    /**
     * Execute the given {@link HttpMethod}.
     *
     * @param method the {@link HttpMethod} to execute
     *
     * @throws IOException if an I/O error occurs
     * @throws HttpException  if an protocol exception occurs
     */
    public synchronized void executeMethod(HttpMethod method) throws IOException, HttpException  {
        if(!connection.isOpen()) {
            connection.open();
        }
        method.execute(state,connection);
        connection.close();
    }

    /**
     * End a session.
     */
    public void endSession()
        throws IOException {
        log.debug("HttpClient.endSession()");
        connection.close();
        connection = null;
        //state = null;
    }

}
