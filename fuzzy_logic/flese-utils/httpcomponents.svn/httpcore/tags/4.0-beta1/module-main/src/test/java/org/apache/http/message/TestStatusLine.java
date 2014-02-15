/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta1/module-main/src/test/java/org/apache/http/message/TestStatusLine.java $
 * $Revision: 604625 $
 * $Date: 2007-12-16 15:11:11 +0100 (Sun, 16 Dec 2007) $
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.http.message;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.StatusLine;

/**
 * Simple tests for {@link StatusLine}.
 *
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * 
 * @version $Id: TestStatusLine.java 604625 2007-12-16 14:11:11Z olegk $
 */
public class TestStatusLine extends TestCase {

    // ------------------------------------------------------------ Constructor
    public TestStatusLine(String testName) {
        super(testName);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestStatusLine.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestStatusLine.class);
    }

    // ------------------------------------------------------ Protected Methods


    // ----------------------------------------------------------- Test Methods

    public void testConstructor() {
        StatusLine statusline = new BasicStatusLine(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK");
        assertEquals(HttpVersion.HTTP_1_1, statusline.getProtocolVersion()); 
        assertEquals(HttpStatus.SC_OK, statusline.getStatusCode()); 
        assertEquals("OK", statusline.getReasonPhrase()); 
    }
        
    public void testConstructorInvalidInput() {
        try {
            new BasicStatusLine(null, HttpStatus.SC_OK, "OK");
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) { /* expected */ }
        try {
            new BasicStatusLine(HttpVersion.HTTP_1_1, -1, "OK");
            fail("IllegalArgumentException should have been thrown");
        } catch (IllegalArgumentException e) { /* expected */ }
    }

    public void testToString() throws Exception {
        StatusLine statusline = new BasicStatusLine(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK");
        assertEquals("HTTP/1.1 200 OK", statusline.toString());
        statusline = new BasicStatusLine(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, null);
        // toString uses default formatting, hence the trailing space
        assertEquals("HTTP/1.1 200 ", statusline.toString());
    }
    
    public void testCloning() throws Exception {
        BasicStatusLine orig = new BasicStatusLine(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK");
        BasicStatusLine clone = (BasicStatusLine) orig.clone();
        assertEquals(orig.getReasonPhrase(), clone.getReasonPhrase());
        assertEquals(orig.getStatusCode(), clone.getStatusCode());
        assertEquals(orig.getProtocolVersion(), clone.getProtocolVersion());
    }
    
}
