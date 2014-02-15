/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/contrib/org/apache/commons/httpclient/contrib/pool/Attic/HttpPoolConnectionManager.java,v 1.1.2.2 2004/07/20 18:04:27 olegk Exp $
 * $Revision: 1.1.2.2 $
 * $Date: 2004-07-20 20:04:27 +0200 (Tue, 20 Jul 2004) $
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
 */
package org.apache.commons.httpclient.contrib.pool;

import java.util.HashMap;
import java.util.Iterator;

import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpConnectionManager;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.pool.impl.GenericObjectPool;

/**
 * <p>
 * DISCLAIMER: HttpClient developers DO NOT actively support this component.
 * The component is provided as a reference material, which may be inappropriate
 * for use without additional customization.
 * </p>
 */
public class HttpPoolConnectionManager implements HttpConnectionManager {

    private HashMap poolsMap = new HashMap();

    private static Log LOG = LogFactory.getLog(HttpPoolConnectionManager.class);

    private static int DEFAULT_MAX_CONNECTIONS = 100;

    HttpPoolConnectionManagerConfiguration config;

    private int maxConnections;

    public HttpPoolConnectionManager() {
        config = new HttpPoolConnectionManagerConfiguration();
        maxConnections = DEFAULT_MAX_CONNECTIONS;
    }

    /**
     * HttpPoolConnectionManager
     * 
     * @param configuration
     *            HttpPoolConnectionManager
     */
    public HttpPoolConnectionManager(HttpPoolConnectionManagerConfiguration configuration) {
        config = configuration;
        maxConnections = DEFAULT_MAX_CONNECTIONS;
    }

    public HttpConnection getConnection(HostConfiguration hostConfiguration) {
        try {
            return getConnection(hostConfiguration, -1);
        } catch (HttpException ex) {
            LOG.error("Failure obtaining HTTP connection", ex);
            return null;
        }
    }

    public HttpConnection getConnection(HostConfiguration hostConfiguration, long timeout)
            throws HttpException {
        LOG.debug("<HttpConnectionManager - getConnection> Inizio metodo");
        //Ottengo la "chiave"
        String key = hostConfiguration.getHost() + ":" + hostConfiguration.getPort();
        LOG.debug("<HttpConnectionManager - getConnection> Pool [" + key + "]");
        GenericObjectPool pool = null;
        synchronized (this) {
            pool = (GenericObjectPool) poolsMap.get(key);
            if (pool == null) {
                //creo pool e ottengo connessione
                LOG.debug("<HttpConnectionManager - getConnection> Pool da creare");
                config.setMaxWait(timeout);
                GenericObjectPool newpool = new GenericObjectPool(
                        new PoolableHttpConnectionFactory(hostConfiguration, this),
                        config.poolconfig);
                poolsMap.put(key, newpool);
                LOG.debug("<HttpConnectionManager - getConnection> Fine metodo");
                try {
                    return (HttpConnection) newpool.borrowObject();
                } catch (Exception ex) {
                    LOG.error("Failure borrowing HTTP connection", ex);
                    throw new HttpException(ex.getMessage());
                }
            }
        }
        //pool in cache
        LOG.debug("<HttpConnectionManager - getConnection> Pool già usato");
        LOG.debug("<HttpConnectionManager - getConnection> Fine metodo");
        try {
            return (HttpConnection) (pool).borrowObject();
        } catch (Exception ex) {
            LOG.error("Error borrowing HTTP connection", ex);
            throw new HttpException(ex.getMessage());
        }
    }

    public void releaseConnection(HttpConnection conn) {
        LOG.debug("<HttpConnectionManager - releaseConnection> Inizio metodo");
        String key = conn.getHost() + ":" + conn.getPort();
        LOG.debug("<HttpConnectionManager - releaseConnection> Pool [" + key + "]");
        if (poolsMap.containsKey(key)) {
            GenericObjectPool pool = (GenericObjectPool) poolsMap.get(key);
            try {
                pool.returnObject(conn);
            } catch (Exception ex) {
                LOG.error("<HttpPoolConnectionManager - releaseConnection> " + ex.toString());
            }
        }
        LOG.debug("<HttpConnectionManager - releaseConnection> Fine metodo");
    }

    public void finalize() {
        LOG.debug("<HttpPoolConnectionManager - finalize> Inizio metodo");
        if (this.poolsMap != null) {
            Iterator keyiter = this.poolsMap.keySet().iterator();
            while (keyiter.hasNext()) {
                String key = (String) keyiter.next();
                LOG.debug("<HttpPoolConnectionManager - finalize> Destroying [" + key + "]");
                GenericObjectPool pool = (GenericObjectPool) this.poolsMap.get(key);
                try {
                    pool.close();
                    this.poolsMap.put(key, null);
                } catch (Exception ex) {
                    LOG.error("Failure closing connection pool", ex);
                }
            }
            this.poolsMap = null;
        }
        LOG.debug("<HttpPoolConnectionManager - finalize> Fine metodo");
    }

    public void shutdown() {
        LOG.debug("<HttpPoolConnectionManager - shutdown> Inizio metodo");
        Iterator keyiter = this.poolsMap.keySet().iterator();
        while (keyiter.hasNext()) {
            String key = (String) keyiter.next();
            LOG.debug("<HttpPoolConnectionManager - shutdown> Destroying [" + key + "]");
            GenericObjectPool pool = (GenericObjectPool) this.poolsMap.get(key);
            try {
                pool.close();
                this.poolsMap.put(key, null);
            } catch (Exception ex) {
                LOG.error("Failure closing connection pool", ex);
            }
        }
        this.poolsMap = null;
        LOG.debug("<HttpPoolConnectionManager - shutdown> Fine metodo");
    }
}