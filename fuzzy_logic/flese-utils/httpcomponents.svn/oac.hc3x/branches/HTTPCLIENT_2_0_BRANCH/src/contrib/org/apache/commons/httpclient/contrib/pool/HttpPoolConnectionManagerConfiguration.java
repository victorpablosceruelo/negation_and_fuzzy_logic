/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/contrib/org/apache/commons/httpclient/contrib/pool/Attic/HttpPoolConnectionManagerConfiguration.java,v 1.1.2.2 2004/07/20 18:04:27 olegk Exp $
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

import org.apache.commons.pool.impl.GenericObjectPool;

/**
 * <p>
 * DISCLAIMER: HttpClient developers DO NOT actively support this component.
 * The component is provided as a reference material, which may be inappropriate
 * for use without additional customization.
 * </p>
 */
public class HttpPoolConnectionManagerConfiguration {

    private final long DEFAULT_TIMEOUT = 2000;

    private final int DEFAULT_MAX_CONNECTIONS_PERHOST = 20;

    private final long DEFAULT_EVICTION_TIME = 1000;

    private final int DEFAULT_NUM_TEST_PEREVICTION = 3;

    protected GenericObjectPool.Config poolconfig;

    /**
     * Default constructor It initializes the pool configuration with default
     * values
     */
    public HttpPoolConnectionManagerConfiguration() {
        poolconfig = new GenericObjectPool.Config();
        //pool configuration
        poolconfig.maxActive = DEFAULT_MAX_CONNECTIONS_PERHOST;
        //eviction thread
        poolconfig.minEvictableIdleTimeMillis = DEFAULT_TIMEOUT;
        poolconfig.whenExhaustedAction = GenericObjectPool.WHEN_EXHAUSTED_BLOCK;
        poolconfig.timeBetweenEvictionRunsMillis = DEFAULT_EVICTION_TIME;
        poolconfig.numTestsPerEvictionRun = DEFAULT_NUM_TEST_PEREVICTION;
        poolconfig.testWhileIdle = true;
    }

    /**
     * Sets the maximum number of connections that could be idle in the pool
     * 
     * @param max
     *            max idle connection in the pool
     */
    public void setMaxIdle(int max) {
        poolconfig.maxIdle = max;
    }

    /**
     * Sets the maximum number of connections that could be idle in the pool
     * 
     * @param min
     *            min idle connection pool
     */
    public void setMinIdle(int min) {
        poolconfig.minIdle = min;
    }

    /**
     * Sets if the connection has to be tested for validity before being
     * returned to the pool
     * 
     * @param test
     *            true if the connection has to be tested before being returned
     *            to the pool
     */
    public void setTestOnReturn(boolean test) {
        poolconfig.testOnReturn = test;
    }

    /**
     * Sets if the connection has to be tested for validity before being
     * borrowed from the pool
     * 
     * @param test
     *            true if the connection has to be tested before being borrowed
     *            from the pool
     */
    public void setTestOnBorrow(boolean test) {
        poolconfig.testOnBorrow = test;
    }

    /**
     * Sets how many milliseconds the client can wait for a connection from the
     * pool
     * 
     * @param wait
     *            maximum wait time
     */
    public void setMaxWait(long wait) {
        poolconfig.maxWait = wait;
    }

    /**
     * Set the maximum number of connection allowed in the pool
     * 
     * @param max
     *            max connection in the pool
     */
    public void setMaxActive(int max) {
        poolconfig.maxActive = max;
    }

    /**
     * Sets how many milliseconds an dle connection could remain in the pool
     * before being closed
     * 
     * @param timeout
     *            maximum idle time
     */
    public void setPoolIdleTime(long timeout) {
        poolconfig.minEvictableIdleTimeMillis = timeout;
    }

    /**
     * Sets the time (in milliseconds) between two runs of the check thred in
     * the pool
     * 
     * @param time
     */
    public void setPoolCheckTime(long time) {
        poolconfig.timeBetweenEvictionRunsMillis = time;
    }

    /**
     * Sets how meny tests have to be perfomrmed during the eviction run
     * 
     * @param test
     *            int
     */
    public void setTestPerCheck(int test) {
        poolconfig.numTestsPerEvictionRun = test;
    }
}