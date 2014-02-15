/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-main/src/test/java/org/apache/http/mockup/HttpDataTransmitterMockup.java $
 * $Revision: 547802 $
 * $Date: 2007-06-15 23:45:59 +0200 (Fri, 15 Jun 2007) $
 *
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

package org.apache.http.mockup;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;

import org.apache.http.impl.io.AbstractHttpDataTransmitter;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpParams;

/**
 * {@link HttpDataTransmitter} mockup implementation.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public class HttpDataTransmitterMockup extends AbstractHttpDataTransmitter {

    private ByteArrayOutputStream buffer = new ByteArrayOutputStream();
    public static int BUFFER_SIZE = 16;
    
    public HttpDataTransmitterMockup(
            final OutputStream outstream, 
            int buffersize,
            final HttpParams params) {
        super();
        init(outstream, buffersize, params);
    }

    public HttpDataTransmitterMockup(
            final OutputStream outstream, 
            int buffersize) {
        this(outstream, buffersize, new BasicHttpParams());
    }

    public HttpDataTransmitterMockup(
            final ByteArrayOutputStream buffer,
            final HttpParams params) {
        this(buffer, BUFFER_SIZE, params);
        this.buffer = buffer;
    }

    public HttpDataTransmitterMockup(
            final ByteArrayOutputStream buffer) {
        this(buffer, BUFFER_SIZE, new BasicHttpParams());
        this.buffer = buffer;
    }

    public HttpDataTransmitterMockup(final HttpParams params) {
        this(new ByteArrayOutputStream(), params);
    }

    public HttpDataTransmitterMockup() {
        this(new ByteArrayOutputStream(), new BasicHttpParams());
    }

    public byte[] getData() {
        if (this.buffer != null) {
            return this.buffer.toByteArray();
        } else {
            return new byte[] {};
        }
    }
    
}
