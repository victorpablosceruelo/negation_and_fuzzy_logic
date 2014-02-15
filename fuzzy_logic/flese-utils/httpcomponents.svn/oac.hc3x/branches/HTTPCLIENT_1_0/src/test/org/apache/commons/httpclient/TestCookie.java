/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestCookie.java,v 1.1 2001/05/01 07:39:35 remm Exp $
 * $Revision: 1.1 $
 * $Date: 2001-05-01 09:39:35 +0200 (Tue, 01 May 2001) $
 *
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


/**
 * Test cases for Cookie
 *
 * @author BC Holmes
 * @version $Revision: 1.1 $
 */
public class TestCookie extends TestCase {


    // -------------------------------------------------------------- Constants


    private static final String DOMAIN_NAME = "www.apache.org";

    private String[] testName = { "custno", "name", "name" };
    private String[] testValue = { "12345", "John", "Doe, John" };
    private String[] testDomain = { "www.apache.org", ".apache.org",
        ".apache.org" };


    // ------------------------------------------------------------ Constructor


    public TestCookie(String name) {
        super(name);
    }


    // ------------------------------------------------------- TestCase Methods


    public static Test suite() {
        return new TestSuite(TestCookie.class);
    }


    // ------------------------------------------------------------ Parse1 Test


    /**
     * Test basic parse (with various spacings
     */
    public void testParse1() throws Exception {
        String headerValue = "custno = 12345; comment=test; version=1," +
            " name=John; version=1; max-age=600; secure; domain=.apache.org";
        Cookie[] cookies = Cookie.parse(DOMAIN_NAME, new Header(
            "set-cookie", headerValue));
        checkResultsOfParse(cookies, 2, 0);
    }


    protected void checkResultsOfParse(
        Cookie[] cookies, int length, int offset) throws Exception {

        assert("number of cookies should be " + length + ", but is " +
               cookies.length + " instead.", cookies.length == length);

        for (int i = 0; i < cookies.length; i++) {

            assert("Name of cookie " + i + " should be \"" +
                   testName[i+offset] + "\", but is " + cookies[i].getName() +
                   " instead.",
                   testName[i+offset].equals(cookies[i].getName()));
            assert("Value of cookie " + i + " should be \"" +
                   testValue[i+offset] + "\", but is " +
                   cookies[i].getValue() + " instead.",
                   testValue[i+offset].equals(cookies[i].getValue()));
            assert("Domain of cookie " + i + " should be \"" +
                   testDomain[i+offset] + "\", but is " +
                   cookies[i].getDomain() + " instead.",
                   testDomain[i+offset].equalsIgnoreCase(
                       cookies[i].getDomain()));
        }
    }


    // ------------------------------------------------------------ Parse2 Test


    /**
     * Test no spaces
     */
    public void testParse2() throws Exception {
        String headerValue = "custno=12345;comment=test; version=1," +
            "name=John;version=1;max-age=600;secure;domain=.apache.org";
        Cookie[] cookies = Cookie.parse(DOMAIN_NAME, new Header(
            "set-cookie", headerValue));
        checkResultsOfParse(cookies, 2, 0);
    }


    // ------------------------------------------------------------ Parse3 Test


    /**
     * Test parse with quoted text
     */
    public void testParse3() throws Exception {
        String headerValue =
            "name=\"Doe, John\";version=1;max-age=600;secure;domain=.apache.org";
        Cookie[] cookies = Cookie.parse(DOMAIN_NAME, new Header(
            "set-cookie", headerValue));
        checkResultsOfParse(cookies, 1, 2);
    }


    /**
     * Test security error
     */
    public void testSecurityError() throws Exception {
        String headerValue = "custno=12345;comment=test; version=1," +
            "name=John;version=1;max-age=600;secure;domain=jakarta.apache.org";
        Exception exception = null;
        try {
            Cookie[] cookies = Cookie.parse(DOMAIN_NAME, new Header(
                "set-cookie", headerValue));
        } catch (HttpException e) {
            exception = e;
        }

        assert("Webdav exception should have been caught", exception != null);
    }


}
