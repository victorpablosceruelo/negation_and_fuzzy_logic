/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/test/java/org/apache/http/conn/TestParams.java $
 * $Revision: 555241 $
 * $Date: 2007-07-11 13:05:31 +0200 (Wed, 11 Jul 2007) $
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

package org.apache.http.conn;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.http.HttpHost;
import org.apache.http.params.HttpParams;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.conn.params.HttpConnectionManagerParams;


/**
 * Unit tests for parameters.
 * Trivial, but it looks better in the Clover reports.
 */
public class TestParams extends TestCase {

    public final static
        HttpHost TARGET1 = new HttpHost("target1.test.invalid");
    public final static
        HostConfiguration HOSTCFG1 = new HostConfiguration(TARGET1,null,null);


    public TestParams(String testName) {
        super(testName);
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestParams.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    public static Test suite() {
        return new TestSuite(TestParams.class);
    }


    public void testBadArgs() {
        HttpParams params = new BasicHttpParams();

/* not checked
        try {
            HttpConnectionManagerParams.
                setMaxConnectionsPerHost(params, null, 3);
            fail();
        } catch (IllegalArgumentException iax) {
            // expected
        }
*/

        try {
            HttpConnectionManagerParams.
                setMaxConnectionsPerHost(null, HOSTCFG1, 3);
            fail();
        } catch (IllegalArgumentException iax) {
            // expected
        }

        try {
            HttpConnectionManagerParams.
                setMaxConnectionsPerHost(params, HOSTCFG1, 0);
            fail();
        } catch (IllegalArgumentException iax) {
            // expected
        }

/* not checked
        try {
            HttpConnectionManagerParams.
                getMaxConnectionsPerHost(params, null);
            fail();
        } catch (IllegalArgumentException iax) {
            // expected
        }
*/

        try {
            HttpConnectionManagerParams.
                getMaxConnectionsPerHost(null, HOSTCFG1);
            fail();
        } catch (IllegalArgumentException iax) {
            // expected
        }


        try {
            HttpConnectionManagerParams.
                setMaxTotalConnections(null, 50);
            fail();
        } catch (IllegalArgumentException iax) {
            // expected
        }

/* not checked
        try {
            HttpConnectionManagerParams.
                setMaxTotalConnections(null, 0);
            fail();
        } catch (IllegalArgumentException iax) {
            // expected
        }
*/

        try {
            HttpConnectionManagerParams.
                getMaxTotalConnections(null);
            fail();
        } catch (IllegalArgumentException iax) {
            // expected
        }
    }


    public void testDefMaxConn() {

        HttpParams params = new BasicHttpParams();

        int fdmcph = HttpConnectionManagerParams.
            getDefaultMaxConnectionsPerHost(params);

        int fmcph = HttpConnectionManagerParams.
            getMaxConnectionsPerHost(params, HOSTCFG1);

        assertEquals(fdmcph, fmcph);

        int dmcph = fdmcph + 3;
        HttpConnectionManagerParams.
            setDefaultMaxConnectionsPerHost(params, dmcph);
        int ndmcph = HttpConnectionManagerParams.
            getDefaultMaxConnectionsPerHost(params);
        assertEquals(dmcph, ndmcph);


        int mcph = HttpConnectionManagerParams.
            getMaxConnectionsPerHost(params, HOSTCFG1);
        assertEquals(ndmcph, mcph);
    }

}
