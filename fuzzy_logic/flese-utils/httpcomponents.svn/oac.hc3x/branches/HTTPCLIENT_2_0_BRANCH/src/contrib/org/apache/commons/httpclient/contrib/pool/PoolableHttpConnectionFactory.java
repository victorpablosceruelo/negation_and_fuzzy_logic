/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/contrib/org/apache/commons/httpclient/contrib/pool/Attic/PoolableHttpConnectionFactory.java,v 1.1.2.2 2004/07/20 18:04:27 olegk Exp $
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

import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.pool.PoolableObjectFactory;

/**
 * <p>
 * DISCLAIMER: HttpClient developers DO NOT actively support this component.
 * The component is provided as a reference material, which may be inappropriate
 * for use without additional customization.
 * </p>
 */
public class PoolableHttpConnectionFactory implements PoolableObjectFactory {

    private HostConfiguration connConf;

    private HttpPoolConnectionManager manager;

    private static Log LOG = LogFactory.getLog(PoolableHttpConnectionFactory.class);

    public PoolableHttpConnectionFactory(
        HostConfiguration hc, HttpPoolConnectionManager manager) {
        connConf = hc;
        this.manager = manager;
    }

    /**
     * Create a new connection using the HostConfiguration passed in the Factory
     * 
     * @return a new connection
     */
    public Object makeObject() {
        LOG.debug("<PoolableHttpConnectionFactory - makeObject> Inizio metodo");
        HttpConnection conn = new HttpConnection(connConf);
        conn.setHttpConnectionManager(manager);
        LOG.debug("<PoolableHttpConnectionFactory - makeObject> Fine metodo");
        return conn;
    }

    public void destroyObject(Object in) {
        LOG.debug("<PoolableHttpConnectionFactory - destroyObject> Inizio metodo");
        if (in != null) {
            LOG.debug("<PoolableHttpConnectionFactory - destroyObject> Connection not null");
            HttpConnection conn = (HttpConnection) in;
            if (conn.isOpen()) {
                LOG.debug("<PoolableHttpConnectionFactory - destroyObject> Closing connection");
                conn.close();
            }
            in = null;
            LOG.debug("<PoolableHttpConnectionFactory - destroyObject> Fine metodo");
        }
    }

    public boolean validateObject(Object in) {
        LOG.debug("<PoolableHttpConnectionFactory - validateObject> Inizio metodo");
        boolean result = false;
        if (in != null) {
            HttpConnection conn = (HttpConnection) in;
            if (conn.isOpen())
                LOG.debug("<PoolableHttpConnectionFactory - validateObject> " +
                        "Connection opened: valid connection");
            result = true;
        }
        LOG.debug("<PoolableHttpConnectionFactory - validateObject> Inizio metodo");
        return result;
    }

    public void activateObject(Object in) {
        LOG.debug("<PoolableHttpConnectionFactory - activateObject> No action");
    }

    public void passivateObject(Object in) {
        LOG.debug("<PoolableHttpConnectionFactory - passivateObject> No action");
    }
}