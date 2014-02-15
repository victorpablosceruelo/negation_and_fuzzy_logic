/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/test/java/org/apache/http/async/TestAbstractHttpHandle.java $
 * $Revision: 505746 $
 * $Date: 2007-02-10 19:59:14 +0100 (Sat, 10 Feb 2007) $
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


import java.io.IOException;

import junit.framework.TestCase;

import org.apache.http.HttpVersion;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpException;
import org.apache.http.message.BasicHttpRequest;
import org.apache.http.message.BasicHttpResponse;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;


public class TestAbstractHttpHandle extends TestCase {

    // public default constructor


    public void testCreateNull() throws Exception {

        MockHttpDispatcher mhd = new MockHttpDispatcher(null);
        HttpRequest        req =
            new BasicHttpRequest("GET", "http://jakarta.apache.org/");
        HttpContext        ctx = new HttpExecutionContext(null);

        // for reference, create with all arguments
        MockHttpHandle mhh = new MockHttpHandle(mhd, req, ctx);
        mhh.checkDispatcher(mhd);
        assertEquals("wrong request", req, mhh.getRequest());
        assertEquals("wrong context", ctx, mhh.getContext());

        // this one is supposed to work
        mhh = new MockHttpHandle(null, req, ctx);
        mhh.checkDispatcher(null);
        assertEquals("wrong request", req, mhh.getRequest());
        assertEquals("wrong context", ctx, mhh.getContext());

        Throwable axe = null;
        mhh = null;
        try {
            mhh = new MockHttpHandle(mhd, null, ctx);
        } catch (Throwable t) {
            axe = t;
        }
        assertNotNull("null request not detected", axe);
        assertTrue("wrong exception class for null request",
                   axe instanceof IllegalArgumentException);

        axe = null;
        mhh = null;
        try {
            mhh = new MockHttpHandle(mhd, req, null);
        } catch (Throwable t) {
            axe = t;
        }
        assertNotNull("null context not detected", axe);
        assertTrue("wrong exception class for null context",
                   axe instanceof IllegalArgumentException);

    } // testCreateNull


    public void testLinked() throws Exception {

        MockHttpHandle mhh = createHandle(null);
        assertTrue("initially not linked", mhh.isLinked());

        mhh.setLinked(true);
        assertTrue("not linked (1)", mhh.isLinked());

        mhh.setLinked(false);
        assertFalse("still linked (1)", mhh.isLinked());

        mhh.setLinked(true);
        assertTrue("not linked (2)", mhh.isLinked());

        mhh.setLinked(false);
        assertFalse("still linked (2)", mhh.isLinked());
    }


    public void testError() throws Exception {

        MockHttpHandle mhh = createHandle(null);
        assertNull("initially with error", mhh.getError());
        mhh.checkError();

        Throwable stone = new IOException();
        mhh.setError(stone);
        assertSame("wrong error (IOX)", stone, mhh.getError());
        Throwable axe = null;
        try {
            mhh.checkError();
        } catch (Throwable t) {
            axe = t;
        }
        assertNotNull("error not checked (IOX)", axe);
        assertSame("wrong error checked (IOX)", stone, axe);

        stone = new HttpException();
        mhh.setError(stone);
        assertSame("wrong error (HTTP)", stone, mhh.getError());
        axe = null;
        try {
            mhh.checkError();
        } catch (Throwable t) {
            axe = t;
        }
        assertNotNull("error not checked (HTTP)", axe);
        assertSame("wrong error checked (HTTP)", stone, axe);

        stone = new NullPointerException();
        mhh.setError(stone);
        assertSame("wrong error (null)", stone, mhh.getError());
        axe = null;
        try {
            mhh.checkError();
        } catch (Throwable t) {
            axe = t;
        }
        assertNotNull("error not checked (null)", axe);
        assertTrue("wrong exception class", axe instanceof HttpException);
        assertSame("wrong error wrapped", stone, axe.getCause());

    } // testError


    public void testCheckDispatcher() throws Exception {

        MockHttpDispatcher mhd1 = new MockHttpDispatcher(null);
        MockHttpDispatcher mhd2 = new MockHttpDispatcher(null);

        MockHttpHandle mhh = createHandle(mhd1);

        mhh.checkDispatcher(mhd1);

        Throwable axe = null;
        try {
            mhh.checkDispatcher(mhd2);
        } catch (Throwable t) {
            axe = t;
        }
        assertNotNull("wrong dispatcher not detected", axe);
        assertTrue("wrong exception class",
                   axe instanceof IllegalArgumentException);

        axe = null;
        try {
            mhh.checkDispatcher(null);
        } catch (Throwable t) {
            axe = t;
        }
        assertNotNull("null dispatcher not detected", axe);
        assertTrue("wrong exception class",
                   axe instanceof IllegalArgumentException);

    } // testCheckDispatcher


    public void testNotificationHandler() throws Exception {

        MockHttpHandle mhh = createHandle(null);
        assertNull("has notification handler",
                   mhh.getNotificationHandler());

        HttpRequest req =
            new BasicHttpRequest("GET", "http://jakarta.apache.org/");

        HttpContext ctx = new HttpExecutionContext(null);
        MockHttpNotificationHandler mhnh = new MockHttpNotificationHandler();
        ctx.setAttribute(HttpNotificationHandler.CTXT_NOTIFICATION_HANDLER,
                         mhnh);

        mhh = new MockHttpHandle(null, req, ctx);
        HttpNotificationHandler hnh = mhh.getNotificationHandler();
        assertNotNull("missing notification handler", hnh);
        assertSame("wrong notification handler", mhnh, hnh);

        // notification handler must not change during handle lifetime
        ctx.removeAttribute(HttpNotificationHandler.CTXT_NOTIFICATION_HANDLER);
        hnh = mhh.getNotificationHandler();
        assertNotNull("vanished notification handler", hnh);
        assertSame("wrong notification handler", mhnh, hnh);

    } // testNotificationHandler


    public void testCloseAbort() throws Exception {

        MockHttpDispatcher mhd = new MockHttpDispatcher(null);

        MockHttpHandle mhh = createHandle(mhd);
        mhh.close();
        assertEquals("close didn't close", 1, mhd.count_close);
        assertEquals("close did abort",    0, mhd.count_abort);

        mhd.reset();
        mhh = createHandle(mhd);
        mhh.abort();
        assertEquals("abort did close",    0, mhd.count_close);
        assertEquals("abort didn't abort", 1, mhd.count_abort);

        // make sure there are no exceptions without dispatcher
        MockHttpHandle mhh0 = createHandle(null);
        mhh0.close();
        mhh0.abort();

    } // testCloseAbort


    public void testPostprocess() throws Exception {

        MockHttpDispatcher mhd = new MockHttpDispatcher(null);
        MockHttpHandle mhh = createHandle(mhd);

        HttpResponse rsp = new BasicHttpResponse
            (HttpVersion.HTTP_1_1, 200, "OK");
        mhh.dispatcherPostprocess(rsp);
        assertEquals("not postprocessed", mhd.count_postprocess, 1);
        assertSame("wrong handle",   mhh, mhd.last_handle);
        assertSame("wrong response", rsp, mhd.last_response);

        // missing dispatcher must not trigger exception
        mhh = createHandle(null);
        mhh.dispatcherPostprocess(rsp);

    } // testPostprocess



    private MockHttpHandle createHandle(AbstractHttpDispatcher dispatcher) {

        HttpRequest req =
            new BasicHttpRequest("GET", "http://jakarta.apache.org/");
        HttpContext ctx =
            new HttpExecutionContext(null);

        return new MockHttpHandle(dispatcher, req, ctx);
    }

} // class TestAbstractHttpHandle
