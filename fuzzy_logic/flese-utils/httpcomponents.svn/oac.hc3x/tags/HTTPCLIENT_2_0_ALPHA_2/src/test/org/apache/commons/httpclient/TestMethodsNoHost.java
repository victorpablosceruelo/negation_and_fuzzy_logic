/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestMethodsNoHost.java,v 1.13 2003/01/23 22:48:27 jsdever Exp $
 * $Revision: 1.13 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
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
import java.io.InputStreamReader;
import java.io.Reader;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.HeadMethod;

/**
 * @author Rodney Waldhoff
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author Ortwin Gl�ck
 * @version $Revision: 1.13 $ $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
 */
public class TestMethodsNoHost extends TestCase {

    static final String NAME = "name", VALUE = "value";
    static final String NAME0 = "name0", VALUE0 = "value0";
    static final String NAME1 = "name1", VALUE1 = "value1";
    static final String NAME2 = "name2", VALUE2 = "value2";
    static final NameValuePair PAIR = new NameValuePair(NAME, VALUE);
    static final NameValuePair PAIR0 = new NameValuePair(NAME0, VALUE0);
    static final NameValuePair PAIR1 = new NameValuePair(NAME1, VALUE1);
    static final NameValuePair PAIR2 = new NameValuePair(NAME2, VALUE2);
    
 

    // ------------------------------------------------------------ Constructor

    public TestMethodsNoHost(String testName) {
        super(testName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestMethodsNoHost.class);
    }

    // ----------------------------------------------------------------- Tests

    public void testPostCantAddParamAfterBodySet() throws Exception {
        PostMethod post = new PostMethod("/foo");
        post.setRequestBody("this+is+the+body");
        try {
            post.addParameter("name","value");
            fail("Expected IllegalStateException");
        } catch(IllegalStateException e) {
            // expected
        }
    }

    /** Ensure setRequestBody fails after addParameter is called. */
    public void testPostCantSetBodyAfterAddParam() throws Exception {
        PostMethod post = new PostMethod("/foo");
        post.addParameter("name","value");
        try {
            post.setRequestBody("this+is+the+body");
            fail("Expected IllegalStateException");
        } catch(IllegalStateException e) {
            // expected
        }
    }


    /** Ensure that parameters can be added and getted. */
    public void testPostAddGetParameter() {
        PostMethod post = new PostMethod();

        //no parameters
        assertNull(post.getParameter("non-existant"));
        assertEquals(0, post.getParameters().length);

        //add one parameter
        post.addParameter(NAME,VALUE);
        assertEquals(PAIR, post.getParameter(NAME));
        assertEquals(1, post.getParameters().length);

        //add another parameter
        post.addParameter(PAIR0);
        assertEquals(PAIR0, post.getParameter(PAIR0.getName()));
        assertEquals(2, post.getParameters().length);

        //add two more parameters
        post.addParameters(new NameValuePair[]{ PAIR1, PAIR2 });
        assertEquals(4, post.getParameters().length);
        NameValuePair[] parameters = post.getParameters();
        for (int i=0; i<parameters.length; i++){
            NameValuePair pair = parameters[i];
            assertEquals(pair, post.getParameter(pair.getName()));
        }
    }

    /** Exercise some unusual and error conditions. */
    public void testPostAddParameterErrorCases() {
        PostMethod post = new PostMethod();

        //add null paramters
        try{
            post.addParameter(null);
            fail("Expected IllegalArgumentException");
        } catch(IllegalArgumentException e) { }

        try{
            post.addParameter(null, "value");
            fail("Expected IllegalArgumentException");
        } catch(IllegalArgumentException e) { }

        try{
            post.addParameter("name", null);
            fail("Expected IllegalArgumentException");
        } catch(IllegalArgumentException e) { }
    }

    /** Adding the same parameter twice should be OK. */
    public void testPostAddSameParameter() {
        PostMethod post = new PostMethod();

        //add same parameter twice
        post.addParameter(PAIR0);
        post.addParameter(PAIR0);

        assertEquals(2, post.getParameters().length);
        NameValuePair[] pairs = post.getParameters();
        assertEquals(pairs[0], pairs[1]);
    }

    public void testPostParametersEncoding() throws IOException {
        PostMethod post = new PostMethod();
        post.addParameter(PAIR);
        assertEquals("name=value", post.getRequestBodyAsString());

        post.addParameters(new NameValuePair[]{ PAIR1, PAIR2 });
        assertEquals("name=value&name1=value1&name2=value2", 
            post.getRequestBodyAsString());

        post.addParameter("hasSpace", "a b c d");
        assertEquals("name=value&name1=value1&name2=value2&hasSpace=a%20b%20c%20d",
            post.getRequestBodyAsString());

    }

    public void testPostSetRequestBody() throws Exception {
        PostMethod post = new PostMethod("/foo");
        String body = "this+is+the+body";
        post.setRequestBody(body);
        assertEquals(body, post.getRequestBodyAsString());
    }


    public void testPostSetRequestBodyThrowsIllegalState() throws Exception {
        PostMethod post = new PostMethod("/foo");
        post.addParameter(NAME0, VALUE0);
        try {
            post.setRequestBody("this+is+the+body");
            fail("Expected IllegalStateException");
        } catch(IllegalStateException e) {
            // expected
        }
    }

    public void testHttpMethodBasePaths() throws Exception {
        HttpMethod simple = new SimpleHttpMethod();
        String[] paths = {
           "/some/absolute/path",
           "../some/relative/path",
           "/",
           "/some/path/with?query=string"
       };
    
        for (int i=0; i<paths.length; i++){
            simple.setPath(paths[i]);
            assertEquals(paths[i], simple.getPath());
        }
    }

    public void testHttpMethodBaseDefaultPath() throws Exception {
        HttpMethod simple = new SimpleHttpMethod();
        assertEquals("/", simple.getPath());

        simple.setPath("");
        assertEquals("/", simple.getPath());

        simple.setPath(null);
        assertEquals("/", simple.getPath());
    }

    public void testHttpMethodBasePathConstructor() throws Exception {
        HttpMethod simple = new SimpleHttpMethod();
        assertEquals("/", simple.getPath());

        simple = new SimpleHttpMethod("");
        assertEquals("/", simple.getPath());

        simple = new SimpleHttpMethod("/some/path/");
        assertEquals("/some/path/", simple.getPath());
    }

    /** Tests response with a Trasfer-Encoding and Content-Length */
    public void testHttpMethodBaseTEandCL() throws Exception {
        SimpleHttpConnection conn = new SimpleHttpConnection();
        String headers = "HTTP/1.1 200 OK\r\n"
                       +"Date: Wed, 28 Mar 2001 05:05:04 GMT\r\n"
                       +"Connection: close\r\n"
                       +"Transfer-Encoding: chunked\r\n"
                       +"Content-Length: 1\r\n";
        String body = "0a\r\n1234567890\r\n3\r\n123\r\n0\r\n";
        conn.addResponse(headers, body);
        conn.open();
        HttpMethodBase method = new GetMethod("/");
        method.execute(new HttpState(), conn);
        String responseBody = method.getResponseBodyAsString();
        // verify that the connection was closed.
        conn.assertNotOpen();
        assertEquals("1234567890123", responseBody);
    }

    public void testConnectionAutoClose() throws Exception {
        SimpleHttpConnection conn = new SimpleHttpConnection();
        String headers = "HTTP/1.1 200 OK\r\n"
                       +"Date: Wed, 28 Mar 2001 05:05:04 GMT\r\n"
                       +"Connection: close\r\n";
        StringBuffer buffer = new StringBuffer(8200);
        for (int i = 0; i < 8200; i++) {
            buffer.append('A');
        }
        String body = buffer.toString();

        conn.addResponse(headers, body);
        conn.open();
        HttpMethodBase method = new GetMethod("/");
        method.execute(new HttpState(), conn);
        Reader response = new InputStreamReader(method.getResponseBodyAsStream());
        int c;
        while ((c = response.read()) != -1) {
           assertEquals((int) 'A', c);
        }
        conn.assertNotOpen();

        // note - this test is here because the HEAD method handler overrides the
        // standard behavior for reading a response body.
        HeadMethod headMethod = new HeadMethod("/");

        conn.addResponse(headers, "");

        try {
            headMethod.execute(new HttpState(), conn);
            conn.assertNotOpen();

        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
    }

    public void testSetGetQueryString1() {
        HttpMethod method = new GetMethod();
        String qs1 = "name1=value1&name2=value2";
        method.setQueryString(qs1);
        assertEquals(qs1, method.getQueryString());
    }

    public void testQueryURIEncoding() {
        HttpMethod method = new GetMethod("http://server/servlet?foo=bar&baz=schmoo");
        assertEquals("foo=bar&baz=schmoo", method.getQueryString());
    }

    public void testSetGetQueryString2() {
        HttpMethod method = new GetMethod();
        NameValuePair[] q1 = new NameValuePair[] {
            new NameValuePair("name1", "value1"),
            new NameValuePair("name2", "value2")
        };
        method.setQueryString(q1);
        String qs1 = "name1=value1&name2=value2";
        assertEquals(qs1, method.getQueryString());
    }

}
