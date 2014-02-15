/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestMethods.java,v 1.1 2001/04/25 18:42:52 remm Exp $
 * $Revision: 1.1 $
 * $Date: 2001-04-25 20:42:48 +0200 (Wed, 25 Apr 2001) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2001 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
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
 */

package org.apache.commons.httpclient;

import java.util.Enumeration;
import junit.framework.*;
import org.apache.commons.httpclient.methods.*;

/**
 * Simple tests for the HTTP client. This test assumes the default 
 * configuration of Tomcat 4 is used as the server.
 * 
 * @author Remy Maucherat
 * @version $Id: TestMethods.java 133417 2001-04-25 18:42:48Z remm $
 */
public class TestMethods extends TestCase {


    // -------------------------------------------------------------- Constants


    private static final String host = "127.0.0.1";
    private static final int port = 8080;


    // ------------------------------------------------------------ Constructor


    public TestMethods(String testName) {
        super(testName);
    }


    // ------------------------------------------------------- TestCase Methods


    public static Test suite() {
        return new TestSuite(TestMethods.class);
    }


    // ----------------------------------------------------------- OPTIONS Test


    public void testMethodsOptions() {

        HttpClient client = new HttpClient();
        OptionsMethod method = new OptionsMethod("/");

        try {
            client.startSession(host, port);
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        Enumeration methodsAllowed = method.getAllowedMethods();
        // This enumeration musn't be empty
        assert("No HTTP method allowed : result of OPTIONS is incorrect.",
               methodsAllowed.hasMoreElements());

    }


    // --------------------------------------------------------------- GET Test


    public void testMethodsGet() {

        HttpClient client = new HttpClient();
        client.startSession(host, port);

        OptionsMethod opmethod = new OptionsMethod("/");

        try {
            client.executeMethod(opmethod);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        GetMethod method = new GetMethod("/");
        method.setUseDisk(false);

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getDataAsString();
            // This enumeration musn't be empty
            assert("No data returned.",
                   (data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        method.recycle();
        method.setPath("/index.html");
        method.setUseDisk(true);

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getDataAsString();
            // This enumeration musn't be empty
            assert("No data returned.",
                   (data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

    }


    // -------------------------------------------------------------- HEAD Test


    public void testMethodsHead() {

        HttpClient client = new HttpClient();
        client.startSession(host, port);

        OptionsMethod opmethod = new OptionsMethod("/");

        try {
            client.executeMethod(opmethod);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        HeadMethod method = new HeadMethod("/");

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        assert("Method failed : " + method.getStatusCode(), 
               (method.getStatusCode() == 200));

        method.recycle();
        method.setPath("/index.html");

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        assert("Method failed : " + method.getStatusCode(), 
               (method.getStatusCode() == 200));

    }


}
