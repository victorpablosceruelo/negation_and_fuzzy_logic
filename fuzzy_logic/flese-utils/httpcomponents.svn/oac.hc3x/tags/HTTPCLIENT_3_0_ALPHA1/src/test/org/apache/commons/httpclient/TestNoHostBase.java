/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestNoHostBase.java,v 1.3 2004/04/24 19:39:24 olegk Exp $
 * $Revision: 1.3 $
 * $Date: 2004-04-24 21:39:24 +0200 (Sat, 24 Apr 2004) $
 *
 * ====================================================================
 *
 *  Copyright 2002-2004 The Apache Software Foundation
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
 */
 package org.apache.commons.httpclient;

import junit.framework.TestCase;

/**
 */
public abstract class TestNoHostBase extends TestCase {

    protected SimpleHttpConnection conn;
    
    protected HttpClient client;
    
    protected NoHostHttpConnectionManager connectionManager;

    /**
     * 
     */
    public TestNoHostBase() {
        super();
    }

    /**
     * @param arg0
     */
    public TestNoHostBase(String arg0) {
        super(arg0);
    }

    public void setUp() throws Exception{
        conn = new SimpleHttpConnection();
        connectionManager = new NoHostHttpConnectionManager();
        connectionManager.setConnection(conn);
        client = new HttpClient(connectionManager);
        client.getHostConfiguration().setHost("localhost", 80, "http");
    }

}
