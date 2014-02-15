/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/SimpleHttpServer.java,v 1.1.2.5 2004/02/22 18:21:18 olegk Exp $
 * $Revision: 1.1.2.5 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2004 The Apache Software Foundation
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient.server;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * A simple, but extensible HTTP server, mostly for testing purposes.
 * 
 * @author Christian Kohlschuetter
 */
public class SimpleHttpServer implements Runnable {
    private static final Log LOG = LogFactory.getLog(SimpleHttpServer.class);
    
    private ServerSocket server = null;
    private Thread t;
    private ThreadGroup tg;
    private boolean stopped = false;

    private Set connections = new HashSet();

    private HttpRequestHandler requestHandler = null;

    /**
     * Creates a new HTTP server instance, using an arbitrary free TCP port
     * 
     * @throws IOException  if anything goes wrong during initialization
     */
    public SimpleHttpServer() throws IOException {
        this(0);
    }

    /**
     * Creates a new HTTP server instance, using the specified TCP port
     * 
     * @param   port    Desired TCP port
     * @throws IOException  if anything goes wrong during initialization
     */
    public SimpleHttpServer(int port) throws IOException {
        server = new ServerSocket(port);
        if(LOG.isInfoEnabled()) {
            LOG.info("New SimpleHttpServer on port " + getLocalPort());
        }
        tg = new ThreadGroup("SimpleHttpServer group");
        t = new Thread(tg, this, "SimpleHttpServer connection handler");
        t.setDaemon(true);
        t.start();
    }

    /**
     * Returns the TCP port that this HTTP server instance is bound to.
     *
     * @return  TCP port, or -1 if not running
     */
    public int getLocalPort() {
        return server.getLocalPort();
    }
    
    /**
     * Returns the IP address that this HTTP server instance is bound to.
     * @return String representation of the IP address or <code>null</code> if not running
     */
    public String getLocalAddress() {
    	return server.getInetAddress().getHostAddress();
    }

    /**
     * Checks if this HTTP server instance is running.
     * 
     * @return  true/false
     */
    public boolean isRunning() {
        if(t == null) {
            return false;
        }
        return t.isAlive();
    }

    /**
     * Stops this HTTP server instance.
     */
    public void destroy() {
        if (stopped) {
            return;
        }

        stopped = true;
        if(LOG.isInfoEnabled()) {
            LOG.info("Stopping SimpleHttpServer on port " + getLocalPort());
        }

        tg.interrupt();

        if (server != null) {
            try {
                server.close();
            } catch(IOException e) {
                
            }
        }

        for (Iterator it = connections.iterator(); it.hasNext();) {
            SimpleHttpServerConnection conn =
                (SimpleHttpServerConnection) it.next();
            conn.destroy();
        }
    }

    /**
     * Returns the currently used HttpRequestHandler by this SimpleHttpServer
     * 
     * @return The used HttpRequestHandler, or null.
     */
    public HttpRequestHandler getRequestHandler() {
        return requestHandler;
    }

    /**
     * Sets the HttpRequestHandler to be used for this SimpleHttpServer.
     * 
     * @param rh    Request handler to be used, or null to disable.
     */
    public void setRequestHandler(HttpRequestHandler rh) {
        requestHandler = rh;
    }

    public void removeConnection(SimpleHttpServerConnection conn) {
        connections.remove(conn);
    }

    public void processRequest(SimpleHttpServerConnection conn)
        throws IOException {

    	boolean complete = false;
        if (requestHandler != null) {
            complete = requestHandler.processRequest(conn);
        }
        if (!complete) {
            conn.connectionClose();
            ErrorResponse.getInstance().getResponse(HttpStatus.SC_SERVICE_UNAVAILABLE).processRequest(conn);
        }
    }

    public void run() {
        try {
            while (!Thread.interrupted()) {
                Socket socket = server.accept();
                try {

                    SimpleHttpServerConnection conn =
                        new SimpleHttpServerConnection(this, socket);

                    connections.add(conn);

                    Thread t =
                        new Thread(tg, conn, "SimpleHttpServer connection");
                    t.setDaemon(true);
                    t.start();
                } catch (IOException e) {
                    LOG.error("SimpleHttpServer error", e);
                    throw new RuntimeException(e.getMessage());
                }
                Thread.sleep(100);
            }
        } catch (InterruptedException accept) {
        } catch (SocketException e) {
            if (!stopped) {
                LOG.error("SimpleHttpServer error", e);
                throw new RuntimeException(e.getMessage());
            }
        } catch (IOException e) {
            LOG.error("SimpleHttpServer error", e);
            throw new RuntimeException(e.getMessage());
        } finally {
            destroy();
        }
    }
}
