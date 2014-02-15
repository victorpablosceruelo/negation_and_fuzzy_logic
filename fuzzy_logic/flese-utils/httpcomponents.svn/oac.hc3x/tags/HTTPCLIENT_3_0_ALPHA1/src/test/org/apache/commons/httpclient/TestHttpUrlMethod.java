/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/TestHttpUrlMethod.java,v 1.6 2004/02/22 18:08:49 olegk Exp $
 * $Revision: 1.6 $
 * $Date: 2004-02-22 19:08:52 +0100 (Sun, 22 Feb 2004) $
 * ====================================================================
 *
 *  Copyright 1999-2004 The Apache Software Foundation
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
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.GetMethod;

/**
 *
 * Unit tests for {@link org.apache.commons.httpclient.HttpMethod}
 * constructors that take URLs. These tests do not require any network
 * connection or web app.
 *
 * @author Marc A. Saegesser
 * @version $Id: TestHttpUrlMethod.java 134530 2004-02-22 18:08:52Z olegk $
 */
public class TestHttpUrlMethod extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestHttpUrlMethod(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestHttpUrlMethod.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestHttpUrlMethod.class);
    }


    // ----------------------------------------------------------- Test Methods

    // Test constructors

    public void testUrlGetMethodWithPathQuery() {
        GetMethod method = new GetMethod("http://www.fubar.com/path1/path2?query=string");
        try {
            assertEquals(
                "Get URL",
                "http://www.fubar.com/path1/path2?query=string",
                method.getURI().toString()
            );
        } catch ( URIException e ) {
            fail( "trouble getting URI: " + e );
        }
        assertEquals("Get Path", "/path1/path2", method.getPath());
        assertEquals("Get query string", "query=string", method.getQueryString());
     
    }

    public void testUrlGetMethodWithPath() {
        GetMethod method = new GetMethod("http://www.fubar.com/path1/path2");
        try {
            assertEquals(
                "Get URL",
                "http://www.fubar.com/path1/path2",
                method.getURI().toString()
            );
        } catch ( URIException e ) {
            fail( "trouble getting URI: " + e );
        }
        assertEquals("Get Path", "/path1/path2", method.getPath());
        assertEquals("Get query string", null, method.getQueryString());

    }

    public void testUrlGetMethod() {
        GetMethod method = new GetMethod("http://www.fubar.com/");
        try {
            assertEquals(
                "Get URL",
                "http://www.fubar.com/",
                method.getURI().toString()
            );
        } catch ( URIException e ) {
            fail( "trouble getting URI: " + e );
        }
        assertEquals("Get Path", "/", method.getPath());
        assertEquals("Get query string", null, method.getQueryString());

    }
    

    public void testUrlGetMethodWithInvalidProtocol() {
        try
        {
            GetMethod method = new GetMethod("crap://www.fubar.com/");
            fail("The use of invalid protocol must have resulted in an IllegalStateException");
        }
        catch(IllegalStateException e)
        {
            //expected exception
        }
    }
}
