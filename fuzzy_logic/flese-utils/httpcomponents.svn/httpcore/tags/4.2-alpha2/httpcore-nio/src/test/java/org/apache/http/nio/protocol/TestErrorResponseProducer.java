/*
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

package org.apache.http.nio.protocol;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import junit.framework.Assert;

import org.apache.http.HttpResponse;
import org.apache.http.HttpVersion;
import org.apache.http.nio.entity.ProducingNHttpEntity;
import org.apache.http.protocol.HTTP;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

public class TestErrorResponseProducer {

    private ErrorResponseProducer erp;
    @Mock private ProducingNHttpEntity entity;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testGenerateResponseKeepAlive() {
        erp = new ErrorResponseProducer(HttpVersion.HTTP_1_1, 200, entity, true);
        HttpResponse res = erp.generateResponse();

        Assert.assertEquals(HTTP.CONN_KEEP_ALIVE, res.getFirstHeader(HTTP.CONN_DIRECTIVE).getValue());
        Assert.assertEquals(entity, res.getEntity());
        Assert.assertEquals(200, res.getStatusLine().getStatusCode());
    }

    @Test
    public void testGenerateResponseClose() {
        erp = new ErrorResponseProducer(HttpVersion.HTTP_1_1, 200, entity, false);
        HttpResponse res = erp.generateResponse();

        Assert.assertEquals(HTTP.CONN_CLOSE, res.getFirstHeader(HTTP.CONN_DIRECTIVE).getValue());
        Assert.assertEquals(entity, res.getEntity());
        Assert.assertEquals(200, res.getStatusLine().getStatusCode());
    }

    @Test
    public void testProduceContent() throws Exception {
        erp = new ErrorResponseProducer(HttpVersion.HTTP_1_1, 200, entity, true);

        erp.produceContent(null, null);

        verify(entity, times(1)).produceContent(null, null);
    }

}
