/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/MultiThreadedHttpConnectionManager.java,v 1.6 2003/01/23 22:47:48 jsdever Exp $
 * $Revision: 1.6 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
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

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Manages a set of HttpConnections for various HostConfigurations.
 *
 * @author <a href="mailto:becke@u.washington.edu">Michael Becke</a>
 * @author Eric Johnson
 *
 * @since 2.0
 */
public class MultiThreadedHttpConnectionManager implements HttpConnectionManager {

    // -------------------------------------------------------- Class Variables
    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(MultiThreadedHttpConnectionManager.class);

    // ----------------------------------------------------- Instance Variables
    private Map mapHosts = new HashMap();
    private int maxConnections = 2;   // Per RFC 2616 sec 8.1.4

    // mapping from reference to hostConfiguration
    private Map referenceToHostConfig;
    // the reference queue used to track when HttpConnections are lost to the
    // garbage collector
    private ReferenceQueue referenceQueue;

    /**
     * No-args constructor
     */
    public MultiThreadedHttpConnectionManager() {

        this.referenceToHostConfig = Collections.synchronizedMap( new HashMap() );
        this.referenceQueue = new ReferenceQueue();

        new ReferenceQueueThread().start();

    }

    /**
     * Sets the maximum number of connections allowed for a given
     * HostConfiguration. Per RFC 2616 section 8.1.4, this value defaults to 2.
     *
     * @param maxConnections the number of connections allowed for each
     * hostConfiguration
     */
    public void setMaxConnectionsPerHost(int maxConnections) {
        this.maxConnections = maxConnections;
    }

    /**
     * Gets the maximum number of connections allowed for a given
     * hostConfiguration.
     *
     * @return The maximum number of connections allowed for a given
     * hostConfiguration.
     */
    public int getMaxConnectionsPerHost() {
        return maxConnections;
    }

    /**
     * @see HttpConnectionManager#getConnection(HostConfiguration)
     */
    public HttpConnection getConnection(HostConfiguration hostConfiguration) {

        while( true ) {
            try {
                return getConnection(hostConfiguration, 0);
            } catch ( HttpException e ) {
                log.debug(
                    "Unexpected exception while waiting for connection",
                    e
                );
            };
        }

    }

    /**
     * @see HttpConnectionManager#getConnection(HostConfiguration, long)
     */
    public HttpConnection getConnection(HostConfiguration hostConfiguration, long timeout)
    throws HttpException {
        log.trace("enter HttpConnectionManager.getConnection(HostConfiguration, long)");

        if (hostConfiguration == null) {
            throw new IllegalArgumentException("hostConfiguration is null");
        }

        if (log.isDebugEnabled()) {
            log.debug("HttpConnectionManager.getConnection:  config = "
                + hostConfiguration + ", timeout = " + timeout);
        }

        // we get the connection pool with a clone of the hostConfiguration
        // so that it cannot be changed once the connecton has been retrieved
        HttpConnection conn = getConnection(
            getConnectionPool( new HostConfiguration( hostConfiguration ) ),
            hostConfiguration,
            timeout
        );

        return conn;
    }

    /**
     * Gets a connection or waits if one is not available.  A connection is
     * available if one exists that is not being used or if fewer than
     * maxConnections have been created in the connectionPool.
     *
     * @param connectionPool
     * @param hostConfiguration
     * @param timeout the number of milliseconds to wait for a connection, 0 to
     * wait indefinitely
     *
     * @return HttpConnection an available connection
     *
     * @throws HttpException if a connection does not available in 'timeout'
     * milliseconds
     */
    private HttpConnection getConnection(
        HostConnectionPool connectionPool,
        HostConfiguration hostConfiguration,
        long timeout
    ) throws HttpException {

        HttpConnection connection = null;

        synchronized( connectionPool ) {

            // keep trying until a connection is available, should happen at
            // most twice
            while ( connection == null ) {

                if (connectionPool.freeConnections.size() > 0) {
                    connection = (HttpConnection)connectionPool.freeConnections.removeFirst();
                } else {
                    // get number of connections hostConfig
                    if (connectionPool.numConnections < maxConnections) {
                        // Create a new connection
                        connection = new HttpConnection( hostConfiguration );
                        connection.setHttpConnectionManager( this );
                        connectionPool.numConnections++;

                        // add a weak reference to this connection
                        referenceToHostConfig.put(
                            new WeakReference( connection, referenceQueue ),
                            hostConfiguration
                        );

                    } else {

                        TimeoutThread threadTimeout = new TimeoutThread();
                        threadTimeout.setTimeout(timeout);
                        threadTimeout.setWakeupThread(Thread.currentThread());
                        threadTimeout.start();

                        try {
                            log.debug(
                                "HttpConnectionManager.getConnection:  waiting for "
                                + "connection from " + connectionPool
                            );
                            connectionPool.wait();
                            // we were woken up before the timeout occurred, so
                            // there should be a connection available
                            threadTimeout.interrupt();
                        } catch (InterruptedException e) {
                            throw new HttpException("Timeout waiting for connection.");
                        }

                    }
                }
            }

        }

        return connection;
    }

    /**
     * Get the pool (list) of connections available for the given hostConfig.
     *
     * @param hostConfiguration the configuraton for the connection pool
     * @return a pool (list) of connections available for the given config
     */
    private HostConnectionPool getConnectionPool(HostConfiguration hostConfiguration) {
        log.trace("enter HttpConnectionManager.getConnections(String)");

        // Look for a list of connections for the given config
        HostConnectionPool listConnections = null;
        synchronized (mapHosts) {
            listConnections = (HostConnectionPool) mapHosts.get(hostConfiguration);
            if (listConnections == null) {
                // First time for this config
                listConnections = new HostConnectionPool();
                mapHosts.put(hostConfiguration, listConnections);
            }
        }
        return listConnections;
    }

    /**
     * Get the number of connections in use for this configuration.
     *
     * @param hostConfiguration the key that connections are tracked on
     * @return the number of connections in use
     */
    public int getConnectionsInUse(HostConfiguration hostConfiguration) {
        log.trace("enter HttpConnectionManager.getConnectionsInUse(String)");

        HostConnectionPool connectionPool = getConnectionPool(hostConfiguration);
        synchronized( connectionPool ) {
            return connectionPool.numConnections;
        }

    }

    /**
     * Make the given HttpConnection available for use by other requests.
     * If another thread is blocked in getConnection() that could use this
     * connection, it will be woken up.
     *
     * @param conn the HttpConnection to make available.
     */
    public void releaseConnection(HttpConnection conn) {
        log.trace("enter HttpConnectionManager.releaseConnection(HttpConnection)");

        // make sure that the response has been read.
        SimpleHttpConnectionManager.finishLastResponse(conn);
        
        HostConfiguration connectionConfiguration = new HostConfiguration();
        connectionConfiguration.setHost(
            conn.getHost(), 
            conn.getPort(), 
            conn.getProtocol()
        );
        if ( conn.getProxyHost() != null ) {
            connectionConfiguration.setProxy(
                conn.getProxyHost(), 
                conn.getProxyPort()
            );
        }

        if(log.isDebugEnabled()){
            log.debug(
                "HttpConnectionManager.releaseConnection:  Release connection for " 
                + connectionConfiguration
            );
        }

        HostConnectionPool listConnections = getConnectionPool(
            connectionConfiguration
        );
        synchronized(listConnections){
            // Put the connect back in the available list and notify a waiter
            listConnections.freeConnections.addFirst(conn);
            if ( listConnections.numConnections == 0 ) {
                // for some reason this connection pool didn't already exist
                log.error(
                    "connection pool not found for: " + connectionConfiguration
                );
                listConnections.numConnections = 1;
            }
            listConnections.notify();
        }
    }

    /**
     * A simple struct-link class to combine the connection list and the count
     * of created connections.
     */
    private class HostConnectionPool {

        public LinkedList freeConnections = new LinkedList();
        public int numConnections = 0;

    }

    /**
     * A thread for listening for HttpConnections reclaimed by the garbage
     * collector.
     */
    private class ReferenceQueueThread extends Thread {

        public ReferenceQueueThread() {
            setDaemon(true);
        }

        /**
         * @see java.lang.Runnable#run()
         */
        public void run() {

            while(true) {

                try {
                    Reference ref = referenceQueue.remove();

                    if ( ref != null ) {
                        HostConfiguration config = (HostConfiguration)
                            referenceToHostConfig.get(ref);
                        referenceToHostConfig.remove(ref);
                        HostConnectionPool connectionPool = getConnectionPool(config);
                        synchronized( connectionPool ) {
                            connectionPool.numConnections--;
                            connectionPool.notify();
                        }
                    }
                } catch (InterruptedException e) {
                    log.debug("ReferenceQueueThread interrupted", e);
                }

            }

        }

    }

    /**
     * In getConnection, if the maximum number of connections has already
     * been reached the call will block.  This class is used to help provide
     * a timeout facility for this wait.  Because Java does not provide a way to
     * determine if wait() returned due to a notify() or a timeout, we need
     * an outside mechanism to interrupt the waiting thread after the specified
     * timeout interval.
     */
    private static class TimeoutThread extends Thread {

        private long timeout = 0;
        private Thread thrdWakeup = null;

        public void setTimeout(long timeout)
        {
            this.timeout = timeout;
        }

        public long getTimeout()
        {
            return timeout;
        }

        public void setWakeupThread(Thread thrdWakeup)
        {
            this.thrdWakeup = thrdWakeup;
        }

        public Thread getWakeupThread()
        {
            return thrdWakeup;
        }

        public void run() {
        log.trace("TimeoutThread.run()");
            if(timeout == 0){
                return;
            }
            if(thrdWakeup == null){
                return;
            }

            try{
                sleep(timeout);
                thrdWakeup.interrupt();
            }catch(InterruptedException e){
            log.debug("InterruptedException caught as expected");
                // This is expected
            }
        }
    }

}
