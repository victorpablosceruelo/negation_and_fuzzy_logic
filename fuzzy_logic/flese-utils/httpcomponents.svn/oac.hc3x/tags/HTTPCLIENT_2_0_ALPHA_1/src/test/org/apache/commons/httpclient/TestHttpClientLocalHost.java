/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestHttpClientLocalHost.java,v 1.1 2001/10/05 18:13:28 rwaldhoff Exp $
 * $Revision: 1.1 $
 * $Date: 2001-10-05 20:13:28 +0200 (Fri, 05 Oct 2001) $
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
import java.util.Enumeration;
import junit.framework.*;
import org.apache.commons.httpclient.methods.*;

/**
 * Simple tests for the HTTP client hitting a local webserver.
 *
 * This test assumes a webserver is running on port 8080 on
 * the 127.0.0.1.
 *
 * The default configuration of Tomcat 4 will work fine.
 *
 * @author Rodney Waldhoff
 * @version $Id: TestHttpClientLocalHost.java 133589 2001-10-05 18:13:28Z rwaldhoff $
 */
public class TestHttpClientLocalHost extends TestCase {


    // -------------------------------------------------------------- Constants


    private static final String host = "127.0.0.1";
    private static final int port = 8080;


    // ------------------------------------------------------------ Constructor


    public TestHttpClientLocalHost(String testName) {
        super(testName);
    }


    // ------------------------------------------------------- TestCase Methods


    public static Test suite() {
        return new TestSuite(TestHttpClientLocalHost.class);
    }

    private HttpClient client = null;
    private GetMethod getSlash = null;
    private GetMethod getSlash2 = null;

    public void setUp() {
        client = new HttpClient();
        getSlash = new GetMethod("/");
    }

    public void tearDown() {
        client = null;
        getSlash = null;
    }

    public void testExecuteMethod() throws Exception {
        client.startSession(host, port);
        assert(200 == client.executeMethod(getSlash));
        String data = getSlash.getResponseBodyAsString();
        assert(null != data);
        assert(data.length() > 0);
        client.endSession();
    }


    public void testExecuteMultipleMethods() throws Exception {
        client.startSession(host, port);
        for(int i=0;i<10;i++) {
            assert(200 == client.executeMethod(getSlash));
            String data = getSlash.getResponseBodyAsString();
            assert(null != data);
            assert(data.length() > 0);
            getSlash.recycle();
            getSlash.setPath("/");
        }
        client.endSession();
    }

}
