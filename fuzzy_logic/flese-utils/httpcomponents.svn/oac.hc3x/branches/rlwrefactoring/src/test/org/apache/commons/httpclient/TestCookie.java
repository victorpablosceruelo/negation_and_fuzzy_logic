/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestCookie.java,v 1.1.2.4 2001/10/01 16:58:05 rwaldhoff Exp $
 * $Revision: 1.1.2.4 $
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import java.util.Date;


/**
 * Test cases for Cookie
 *
 * @author BC Holmes
 * @author Rod Waldhoff
 * @version $Revision: 1.1.2.4 $
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
        Cookie[] cookies = Cookie.parse(DOMAIN_NAME,"/", true, new Header(
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
        Cookie[] cookies = Cookie.parse(DOMAIN_NAME, "/", true, new Header(
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
        Cookie[] cookies = Cookie.parse(DOMAIN_NAME,"/", true, new Header(
            "set-cookie", headerValue));
        checkResultsOfParse(cookies, 1, 2);
    }

    // ------------------------------------------------------------- More Tests

    public void testSecurityError() throws Exception {
        String headerValue = "custno=12345;comment=test; version=1," +
            "name=John;version=1;max-age=600;secure;domain=jakarta.apache.org";
        try {
            Cookie[] cookies = Cookie.parse(DOMAIN_NAME, "/", new Header(
                "set-cookie", headerValue));
            fail("HttpException exception should have been thrown");
        } catch (HttpException e) {
            // expected
        }
    }

    public void testParseSimple() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/",setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assertEquals("Value","cookie-value",parsed[0].getValue());
        assert("Comment",null == parsed[0].getComment());
        assert("ExpiryDate",null == parsed[0].getExpiryDate());
        //assert("isToBeDiscarded",parsed[0].isToBeDiscarded());
        assert("isPersistent",!parsed[0].isPersistent());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/",parsed[0].getPath());
        assert("Secure",!parsed[0].getSecure());
        assertEquals("Version",1,parsed[0].getVersion());
    }

    public void testParseNoName() throws Exception {
        Header setCookie = new Header("Set-Cookie","=cookie-value");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/",setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","",parsed[0].getName());
        assertEquals("Value","cookie-value",parsed[0].getValue());
        assert("Comment",null == parsed[0].getComment());
        assert("ExpiryDate",null == parsed[0].getExpiryDate());
        //assert("isToBeDiscarded",parsed[0].isToBeDiscarded());
        assert("isPersistent",!parsed[0].isPersistent());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/",parsed[0].getPath());
        assert("Secure",!parsed[0].getSecure());
        assertEquals("Version",1,parsed[0].getVersion());
    }

    public void testParseNoValue() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/",setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assert("Value",null == parsed[0].getValue());
        assert("Comment",null == parsed[0].getComment());
        assert("ExpiryDate",null == parsed[0].getExpiryDate());
        //assert("isToBeDiscarded",parsed[0].isToBeDiscarded());
        assert("isPersistent",!parsed[0].isPersistent());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/",parsed[0].getPath());
        assert("Secure",!parsed[0].getSecure());
        assertEquals("Version",1,parsed[0].getVersion());
    }

    public void testParseWithWhiteSpace() throws Exception {
        Header setCookie = new Header("Set-Cookie"," cookie-name  =    cookie-value  ");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/",setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assertEquals("Value","cookie-value",parsed[0].getValue());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/",parsed[0].getPath());
        assert("Secure",!parsed[0].getSecure());
        assert("ExpiryDate",null == parsed[0].getExpiryDate());
        assert("Comment",null == parsed[0].getComment());
    }

    public void testParseWithQuotes() throws Exception {
        Header setCookie = new Header("Set-Cookie"," cookie-name  =  \" cookie-value \" ;path=/");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/",setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assertEquals("Value"," cookie-value ",parsed[0].getValue());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/",parsed[0].getPath());
        assert("Secure",!parsed[0].getSecure());
        assert("ExpiryDate",null == parsed[0].getExpiryDate());
        assert("Comment",null == parsed[0].getComment());
    }

    public void testParseWithPath() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value; Path=/path/");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/path/",setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assertEquals("Value","cookie-value",parsed[0].getValue());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/path/",parsed[0].getPath());
        assert("Secure",!parsed[0].getSecure());
        assert("ExpiryDate",null == parsed[0].getExpiryDate());
        assert("Comment",null == parsed[0].getComment());
    }

    public void testParseWithDomain() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value; Domain=127.0.0.1");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/",setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assertEquals("Value","cookie-value",parsed[0].getValue());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/",parsed[0].getPath());
        assert("Secure",!parsed[0].getSecure());
        assert("ExpiryDate",null == parsed[0].getExpiryDate());
        assert("Comment",null == parsed[0].getComment());
    }

    public void testParseWithSecure() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value; secure");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/",true,setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assertEquals("Value","cookie-value",parsed[0].getValue());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/",parsed[0].getPath());
        assert("Secure",parsed[0].getSecure());
        assert("ExpiryDate",null == parsed[0].getExpiryDate());
        assert("Comment",null == parsed[0].getComment());
    }

    public void testParseWithComment() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value; comment=\"This is a comment.\"");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/",true,setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assertEquals("Value","cookie-value",parsed[0].getValue());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/",parsed[0].getPath());
        assert("Secure",!parsed[0].getSecure());
        assert("ExpiryDate",null == parsed[0].getExpiryDate());
        assertEquals("Comment","This is a comment.",parsed[0].getComment());
    }

    public void testParseWithExpires() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value;Expires=Thu, 01-Jan-1970 00:00:10 GMT");
        Cookie[] parsed = Cookie.parse("127.0.0.1","/",true,setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assertEquals("Value","cookie-value",parsed[0].getValue());
        assertEquals("Domain","127.0.0.1",parsed[0].getDomain());
        assertEquals("Path","/",parsed[0].getPath());
        assert("Secure",!parsed[0].getSecure());
        assertEquals(new Date(10000L),parsed[0].getExpiryDate());
        assert("Comment",null == parsed[0].getComment());
    }

    public void testParseWithAll() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value;Path=/commons;Domain=.apache.org;Comment=This is a comment.;secure;Expires=Thu, 01-Jan-1970 00:00:10 GMT");
        Cookie[] parsed = Cookie.parse(".apache.org","/commons/httpclient",true,setCookie);
        assertEquals("Found 1 cookie.",1,parsed.length);
        assertEquals("Name","cookie-name",parsed[0].getName());
        assertEquals("Value","cookie-value",parsed[0].getValue());
        assertEquals("Domain",".apache.org",parsed[0].getDomain());
        assertEquals("Path","/commons",parsed[0].getPath());
        assert("Secure",parsed[0].getSecure());
        assertEquals(new Date(10000L),parsed[0].getExpiryDate());
        assertEquals("Comment","This is a comment.",parsed[0].getComment());
    }

    public void testParseWithWrongDomain() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value; domain=127.0.0.1");
        try {
            Cookie[] parsed = Cookie.parse("127.0.0.2","/",setCookie);
            fail("HttpException exception should have been thrown");
        } catch (HttpException e) {
            // expected
        }
    }

    public void testParseWithWrongPath() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value; domain=127.0.0.1; path=/not/just/root");
        try {
            Cookie[] parsed = Cookie.parse("127.0.0.1","/",setCookie);
            fail("HttpException exception should have been thrown");
        } catch (HttpException e) {
            // expected
        }
    }

    public void testParseWithWrongSecure() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value; domain=127.0.0.1; path=/; secure");
        try {
            Cookie[] parsed = Cookie.parse("127.0.0.1","/",setCookie);
            fail("HttpException exception should have been thrown");
        } catch (HttpException e) {
            // expected
        }
    }

    public void testParseWithWrongSecure2() throws Exception {
        Header setCookie = new Header("Set-Cookie","cookie-name=cookie-value; domain=127.0.0.1; path=/; secure");
        try {
            Cookie[] parsed = Cookie.parse("127.0.0.1","/",false,setCookie);
            fail("HttpException exception should have been thrown");
        } catch (HttpException e) {
            // expected
        }
    }
}
