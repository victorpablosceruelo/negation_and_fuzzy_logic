/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestWebappMultiPostMethod.java,v 1.4.2.1 2004/02/22 18:21:16 olegk Exp $
 * $Revision: 1.4.2.1 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
 * ====================================================================
 *
 *  Copyright 2003-2004 The Apache Software Foundation
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.MultipartPostMethod;
import org.apache.commons.httpclient.methods.multipart.ByteArrayPartSource;
import org.apache.commons.httpclient.methods.multipart.FilePart;
import org.apache.commons.httpclient.methods.multipart.StringPart;

/**
 * Webapp tests specific to the MultiPostMethod.
 *
 * @author <a href="oleg@ural.ru">Oleg Kalnichevski</a>
 */
public class TestWebappMultiPostMethod extends TestWebappBase {

    HttpClient httpClient; 
    final String paramsPath = "/" + getWebappContext() + "/params";
    final String bodyPath = "/" + getWebappContext() + "/body";

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
        httpClient = createHttpClient();
    }

    // ------------------------------------------------------------------ Tests
    
    /**
     * Test that the body consisting of a string part can be posted.
     */

    public void testPostStringPart() throws Exception {
        MultipartPostMethod method = new MultipartPostMethod(bodyPath);
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
        MultipartPostMethod method = new MultipartPostMethod(bodyPath);
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

