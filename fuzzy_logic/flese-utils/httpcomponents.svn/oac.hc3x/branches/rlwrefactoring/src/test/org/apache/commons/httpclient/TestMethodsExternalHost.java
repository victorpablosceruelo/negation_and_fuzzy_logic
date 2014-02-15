/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestMethodsExternalHost.java,v 1.1.2.2 2001/10/01 16:58:05 rwaldhoff Exp $
 * $Revision: 1.1.2.2 $
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
import java.util.Enumeration;
import junit.framework.*;
import org.apache.commons.httpclient.methods.*;

/**
 * Simple tests for the HTTP client hitting an external webserver.
 *
 * This test suite assumes you have an internet connection that
 * can communicate with http://java.sun.com/.
 *
 * @author Remy Maucherat
 * @author Rodney Waldhoff
 * @version $Id: TestMethodsExternalHost.java 133579 2001-10-01 16:58:05Z rwaldhoff $
 */
public class TestMethodsExternalHost extends TestCase {


    // -------------------------------------------------------------- Constants

    private static final String externalHost = "java.sun.com";
    private static final int externalPort = 80;
    private static final String externalPath = "/index.html";

    // ------------------------------------------------------------ Constructor


    public TestMethodsExternalHost(String testName) {
        super(testName);
    }


    // ------------------------------------------------------- TestCase Methods


    public static Test suite() {
        return new TestSuite(TestMethodsExternalHost.class);
    }


    // ----------------------------------------------------------- OPTIONS Test


    public void testMethodsOptionsExternal() {

        HttpClient client = new HttpClient();
        client.startSession(externalHost, externalPort);
        OptionsMethod method = new OptionsMethod(externalPath);

        try {
            client.startSession(externalHost, externalPort);
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


    public void testMethodsGetExternal() {

        HttpClient client = new HttpClient();
        client.startSession(externalHost, externalPort);

        GetMethod method = new GetMethod("/");
        method.setUseDisk(false);

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            // This enumeration musn't be empty
            assert("No data returned.",
                   (data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        method.recycle();
        method.setPath(externalPath);
        method.setUseDisk(true);

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            // This enumeration musn't be empty
            assert("No data returned.",
                   (data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

    }


    // -------------------------------------------------------------- HEAD Test

    public void testMethodsHeadExternal() {

        HttpClient client = new HttpClient();
        client.startSession(externalHost, externalPort);

        HeadMethod method = new HeadMethod(externalPath);

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        assert("Method failed : " + method.getStatusCode(),
               (method.getStatusCode() == 200));

    }

    /**
     * This test proves that bad urls throw an IOException,
     * and not some other throwable like a NullPointerException.
     */
    public void testIOException() {

        HttpClient client = new HttpClient();
        client.startSession("http://whaturl.com.org", externalPort);

        GetMethod method = new GetMethod("");

        try {
            client.executeMethod(method);
        } catch (IOException e) {
            return; // good
        } catch (HttpException e) {
            return; // also good
        }

        fail("Should have thrown an exception");

    }

}
