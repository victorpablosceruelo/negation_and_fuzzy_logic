/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestWebappMultiPostMethod.java,v 1.2 2003/02/20 03:14:01 jsdever Exp $
 * $Revision: 1.2 $
 * $Date: 2003-02-20 04:14:01 +0100 (Thu, 20 Feb 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2003 The Apache Software Foundation.  All rights
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient;

import junit.framework.*;
import org.apache.commons.httpclient.methods.*;
import org.apache.commons.httpclient.methods.multipart.*;
import java.io.*;

/**
 * Webapp tests specific to the MultiPostMethod.
 *
 * @author <a href="oleg@ural.ru">Oleg Kalnichevski</a>
 */
public class TestWebappMultiPostMethod extends TestWebappBase {

    HttpClient httpClient; 
    final static String paramsUrl = "http://" + host + ":" + port
        + "/" + context + "/params";
    final static String bodyUrl = "http://" + host + ":" + port
        + "/" + context + "/body";

    public TestWebappMultiPostMethod(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestWebappMultiPostMethod.class);
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestWebappMultiPostMethod.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    public void setUp() {
        httpClient = new HttpClient();
    }

    // ------------------------------------------------------------------ Tests
    
    /**
     * Test that the body consisting of a string part can be posted.
     */

    public void testPostStringPart() throws Exception {
        MultipartPostMethod method = new MultipartPostMethod(bodyUrl);
        method.addPart(new StringPart("param", "Hello", "ISO-8859-1"));

        httpClient.executeMethod(method);

        assertEquals(200,method.getStatusCode());
        String body = method.getResponseBodyAsString();
        assertTrue(body.indexOf("Content-Disposition: form-data; name=\"param\"") >= 0);
        assertTrue(body.indexOf("Content-Type: text/plain; charset=ISO-8859-1") >= 0);
        assertTrue(body.indexOf("Content-Transfer-Encoding: 8bit") >= 0);
        assertTrue(body.indexOf("Hello") >= 0);
    }


    /**
     * Test that the body consisting of a file part can be posted.
     */
    public void testPostFilePart() throws Exception {
        MultipartPostMethod method = new MultipartPostMethod(bodyUrl);
        byte[] content = "Hello".getBytes();
        method.addPart(
          new FilePart(
            "param1", 
            new ByteArrayPartSource("filename.txt", content), 
            "text/plain", 
            "ISO-8859-1"));

        httpClient.executeMethod(method);

        assertEquals(200,method.getStatusCode());
        String body = method.getResponseBodyAsString();
        assertTrue(body.indexOf("Content-Disposition: form-data; name=\"param1\"; filename=\"filename.txt\"") >= 0);
        assertTrue(body.indexOf("Content-Type: text/plain; charset=ISO-8859-1") >= 0);
        assertTrue(body.indexOf("Content-Transfer-Encoding: binary") >= 0);
        assertTrue(body.indexOf("Hello") >= 0);
    }
}

