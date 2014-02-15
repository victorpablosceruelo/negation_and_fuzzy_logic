/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/test/java/org/apache/http/async/TestAbstractHttpDispatcher.java $
 * $Revision: 489367 $
 * $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
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

package org.apache.http.async;

import java.util.List;
import java.util.ArrayList;

import junit.framework.TestCase;

import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.message.BasicHttpRequest;
import org.apache.http.protocol.BasicHttpProcessor;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;
import org.apache.http.protocol.HttpProcessor;

public class TestAbstractHttpDispatcher extends TestCase {

    public void testCreateNull() throws Exception {
        AbstractHttpDispatcher ahd = new MockHttpDispatcher(null);
        assertNotNull("no collection of handles", ahd.linked_handles);
    }


    public void testCreateList() throws Exception {
        List l = new ArrayList();
        AbstractHttpDispatcher ahd = new MockHttpDispatcher(l);
        assertNotNull("no collection of handles", ahd.linked_handles);
        assertTrue("wrong collection of handles", ahd.linked_handles == l);
    }


    public void testPrepareContext() throws Exception {
        HttpProcessor processor = new BasicHttpProcessor();
        MockAsyncHttpProcessor mahp = new MockAsyncHttpProcessor(processor);
        HttpHost target = new HttpHost("127.0.0.1", 80);
        HttpContext ctxt = null;

        try {
            ctxt = AbstractHttpDispatcher.prepareContext(null, target, null);
            fail("null processor not detected");
        } catch (Throwable t) {
            assertTrue("wrong exception class",
                       t instanceof IllegalArgumentException);
        }
        
        try {
            ctxt = AbstractHttpDispatcher.prepareContext(mahp, null, null);
            fail("null target not detected");
        } catch (Throwable t) {
            assertTrue("wrong exception class",
                       t instanceof IllegalArgumentException);
        }
        
        ctxt = AbstractHttpDispatcher.prepareContext(mahp, target, null);
        assertNotNull("no context created", ctxt);

        HttpContext parent = new HttpExecutionContext(null);
        parent.setAttribute("test", "testvalue");
        ctxt = AbstractHttpDispatcher.prepareContext(mahp, target, parent);
        assertNotNull("no context returned", ctxt);
        assertNotSame("no context created", parent, ctxt);
        assertEquals("wrong attribute in context",
                     "testvalue", ctxt.getAttribute("test"));

        assertEquals("prepare invoked", 0, mahp.count_prepare);
        assertEquals("send invoked",    0, mahp.count_send);
        assertEquals("receive invoked", 0, mahp.count_receive);
        assertEquals("finish invoked",  0, mahp.count_finish);
    }


    public void testPrepare() throws Exception {
        HttpProcessor processor = new BasicHttpProcessor();
        MockAsyncHttpProcessor mahp = new MockAsyncHttpProcessor(processor);
        HttpHost target = new HttpHost("127.0.0.1", 80);
        HttpRequest req =
            new BasicHttpRequest("GET", "http://127.0.0.1/");
        HttpContext ctxt =
            AbstractHttpDispatcher.prepareContext(mahp, target, null);

        try {
            AbstractHttpDispatcher.prepareRequest(null, req, ctxt);
            fail("null processor not detected");
        } catch (Throwable t) {
            assertTrue("wrong exception class",
                       t instanceof IllegalArgumentException);
        }

        try {
            AbstractHttpDispatcher.prepareRequest(mahp, null, ctxt);
            fail("null request not detected");
        } catch (Throwable t) {
            assertTrue("wrong exception class",
                       t instanceof IllegalArgumentException);
        }

        try {
            AbstractHttpDispatcher.prepareRequest(mahp, req, null);
            fail("null context not detected");
        } catch (Throwable t) {
            assertTrue("wrong exception class",
                       t instanceof IllegalArgumentException);
        }

        AbstractHttpDispatcher.prepareRequest(mahp, req, ctxt);
        assertEquals("prepare not invoked", 1, mahp.count_prepare);
        assertEquals("send invoked",        0, mahp.count_send);
        assertEquals("receive invoked",     0, mahp.count_receive);
        assertEquals("finish invoked",      0, mahp.count_finish);

        try {
            AbstractHttpDispatcher.prepareRequest(mahp, req, ctxt);
            fail("double preprocessing not detected");
        } catch (Throwable t) {
            assertTrue("wrong exception class",
                       t instanceof IllegalArgumentException);
        }
    }


    public void testTransmitRequestTo() throws Exception {
        HttpProcessor processor = new BasicHttpProcessor();
        MockAsyncHttpProcessor mahp = new MockAsyncHttpProcessor(processor);
        AbstractHttpDispatcher.transmitRequestTo(mahp, null, null, null);
        assertEquals("prepare invoked",     0, mahp.count_prepare);
        assertEquals("send not invoked",    1, mahp.count_send);
        assertEquals("receive invoked",     0, mahp.count_receive);
        assertEquals("finish invoked",      0, mahp.count_finish);
    }


    public void testObtainResponse() throws Exception {
        HttpProcessor processor = new BasicHttpProcessor();
        MockAsyncHttpProcessor mahp = new MockAsyncHttpProcessor(processor);
        AbstractHttpDispatcher.obtainResponse(mahp, null, null, null);
        assertEquals("prepare invoked",     0, mahp.count_prepare);
        assertEquals("send invoked",        0, mahp.count_send);
        assertEquals("receive not invoked", 1, mahp.count_receive);
        assertEquals("finish invoked",      0, mahp.count_finish);
    }


    public void testFinishResponse() throws Exception {
        HttpProcessor processor = new BasicHttpProcessor();
        MockAsyncHttpProcessor mahp = new MockAsyncHttpProcessor(processor);
        AbstractHttpDispatcher.finishResponse(mahp, null, null);
        assertEquals("prepare invoked",     0, mahp.count_prepare);
        assertEquals("send invoked",        0, mahp.count_send);
        assertEquals("receive invoked",     0, mahp.count_receive);
        assertEquals("finish not invoked",  1, mahp.count_finish);
    }

} // class TestAbstractHttpDispatcher
