/*
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


import org.apache.commons.httpclient.protocol.Protocol; 
import junit.framework.*;

/**
 * Simple tests for {@link StatusLine}.
 *
 * @author <a href="mailto:oleg@ural.ru">oleg Kalnichevski</a>
 * @version $Id: TestRequestLine.java 134531 2004-02-22 18:21:18Z olegk $
 */
public class TestRequestLine extends TestCase {

    private StatusLine statusLine = null;

    // ------------------------------------------------------------ Constructor
    public TestRequestLine(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestRequestLine.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestRequestLine.class);
    }

    // ------------------------------------------------------ Protected Methods


    // ----------------------------------------------------------- Test Methods

    public void testRequestLineGeneral() throws Exception {
        SimpleHttpConnection conn = null;
        SimpleHttpMethod method = null;
                
        conn = new SimpleHttpConnection(null, -1, "localhost", null, 80, Protocol.getProtocol("http"));

        method = new SimpleHttpMethod();
        assertEquals("Simple / HTTP/1.1\r\n", method.getTestRequestLine(conn));

        method = new SimpleHttpMethod("stuff");
        assertEquals("Simple stuff HTTP/1.1\r\n", method.getTestRequestLine(conn));

        conn = new SimpleHttpConnection("proxy", 8080, "localhost", null, 80, Protocol.getProtocol("http"));

        method = new SimpleHttpMethod();
        assertEquals("Simple http://localhost/ HTTP/1.1\r\n", method.getTestRequestLine(conn));

        method = new SimpleHttpMethod("stuff");
        assertEquals("Simple http://localhost/stuff HTTP/1.1\r\n", method.getTestRequestLine(conn));

        conn = new SimpleHttpConnection("proxy", 8080, "localhost", null, -1, Protocol.getProtocol("http"));

        method = new SimpleHttpMethod();
        assertEquals("Simple http://localhost/ HTTP/1.1\r\n", method.getTestRequestLine(conn));

        method = new SimpleHttpMethod("stuff");
        assertEquals("Simple http://localhost/stuff HTTP/1.1\r\n", method.getTestRequestLine(conn));

        conn = new SimpleHttpConnection("proxy", 8080, "localhost", null, 666, Protocol.getProtocol("http"));

        method = new SimpleHttpMethod();
        assertEquals("Simple http://localhost:666/ HTTP/1.1\r\n", method.getTestRequestLine(conn));

        method = new SimpleHttpMethod("stuff");
        assertEquals("Simple http://localhost:666/stuff HTTP/1.1\r\n", method.getTestRequestLine(conn));
    }

    public void testRequestLineQuery() throws Exception {
        SimpleHttpConnection conn = null;
        SimpleHttpMethod method = null;
                
        conn = new SimpleHttpConnection(null, -1, "localhost", null, 80, Protocol.getProtocol("http"));

        method = new SimpleHttpMethod();
        method.setQueryString( new NameValuePair[] {
            new NameValuePair("param1", " !#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~"),
            new NameValuePair("param2", "some stuff")
          } );
        assertEquals("Simple /?param1=+%21%23%24%25%26%27%28%29*%2B%2C-.%2F%3A%3B%3C%3D%3E%3F%40%5B%5C%5D%5E_%60%7B%7C%7D%7E&param2=some+stuff HTTP/1.1\r\n", 
          method.getTestRequestLine(conn));
    }

    public void testRequestLinePath() throws Exception {
        SimpleHttpConnection conn = null;
        SimpleHttpMethod method = null;
                
        conn = new SimpleHttpConnection(null, -1, "localhost", null, 80, Protocol.getProtocol("http"));

        method = new SimpleHttpMethod();
        method.setPath("/some%20stuff/");
        assertEquals("Simple /some%20stuff/ HTTP/1.1\r\n", 
          method.getTestRequestLine(conn));

        method = new SimpleHttpMethod("/some%20stuff/");
        assertEquals("Simple /some%20stuff/ HTTP/1.1\r\n", 
          method.getTestRequestLine(conn));
    }
}
