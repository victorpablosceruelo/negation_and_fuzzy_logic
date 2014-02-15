/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/test/java/org/apache/http/impl/entity/TestStrictContentLengthStrategy.java $
 * $Revision: 472564 $
 * $Date: 2006-11-08 18:59:20 +0100 (Wed, 08 Nov 2006) $
 * ====================================================================
 *
 *  Copyright 2002-2004 The Apache Software Foundation
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
 */

package org.apache.http.impl.entity;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.http.HttpMessage;
import org.apache.http.HttpVersion;
import org.apache.http.ProtocolException;
import org.apache.http.entity.ContentLengthStrategy;
import org.apache.http.mockup.HttpMessageMockup;
import org.apache.http.params.HttpProtocolParams;

public class TestStrictContentLengthStrategy extends TestCase {

    public TestStrictContentLengthStrategy(String testName) {
        super(testName);
    }

    // ------------------------------------------------------- TestCase Methods

    public static Test suite() {
        return new TestSuite(TestStrictContentLengthStrategy.class);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestStrictContentLengthStrategy.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    public void testEntityWithChunkTransferEncoding() throws Exception {
        ContentLengthStrategy lenStrategy = new StrictContentLengthStrategy();
        HttpMessage message = new HttpMessageMockup();
        message.addHeader("Transfer-Encoding", "Chunked");

        assertEquals(ContentLengthStrategy.CHUNKED, lenStrategy.determineLength(message));
    }

    public void testEntityWithIdentityTransferEncoding() throws Exception {
        ContentLengthStrategy lenStrategy = new StrictContentLengthStrategy();
        HttpMessage message = new HttpMessageMockup();
        message.addHeader("Transfer-Encoding", "Identity");

        assertEquals(ContentLengthStrategy.IDENTITY, lenStrategy.determineLength(message));
    }
    
    public void testEntityWithInvalidTransferEncoding() throws Exception {
        ContentLengthStrategy lenStrategy = new StrictContentLengthStrategy();
        HttpMessage message = new HttpMessageMockup();
        message.addHeader("Transfer-Encoding", "whatever");

        try {
            lenStrategy.determineLength(message);
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException ex) {
            // expected
        }
    }
    
    public void testEntityWithInvalidChunkEncodingAndHTTP10() throws Exception {
        ContentLengthStrategy lenStrategy = new StrictContentLengthStrategy();
        HttpMessage message = new HttpMessageMockup();
        message.getParams().setParameter(HttpProtocolParams.PROTOCOL_VERSION, 
                HttpVersion.HTTP_1_0);
        message.addHeader("Transfer-Encoding", "chunked");

        try {
            lenStrategy.determineLength(message);
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException ex) {
            // expected
        }
    }
    
    public void testEntityWithContentLength() throws Exception {
        ContentLengthStrategy lenStrategy = new StrictContentLengthStrategy();
        HttpMessage message = new HttpMessageMockup();
        message.addHeader("Content-Length", "100");
        assertEquals(100, lenStrategy.determineLength(message));
    }
    
    public void testEntityWithInvalidContentLength() throws Exception {
        ContentLengthStrategy lenStrategy = new StrictContentLengthStrategy();
        HttpMessage message = new HttpMessageMockup();
        message.addHeader("Content-Length", "whatever");

        try {
            lenStrategy.determineLength(message);
            fail("ProtocolException should have been thrown");
        } catch (ProtocolException ex) {
            // expected
        }
    }

    public void testEntityNoContentDelimiter() throws Exception {
        ContentLengthStrategy lenStrategy = new StrictContentLengthStrategy();
        HttpMessage message = new HttpMessageMockup();
        assertEquals(ContentLengthStrategy.IDENTITY, lenStrategy.determineLength(message));
    }
        
}

