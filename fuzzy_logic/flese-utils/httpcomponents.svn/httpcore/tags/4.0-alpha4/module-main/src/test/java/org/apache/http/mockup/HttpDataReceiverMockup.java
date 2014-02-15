/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-main/src/test/java/org/apache/http/mockup/HttpDataReceiverMockup.java $
 * $Revision: 496069 $
 * $Date: 2007-01-14 13:03:05 +0100 (Sun, 14 Jan 2007) $
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import org.apache.http.impl.io.AbstractHttpDataReceiver;

/**
 * {@link HttpDataInputStream} mockup implementation.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public class HttpDataReceiverMockup extends AbstractHttpDataReceiver {

    public static int BUFFER_SIZE = 16;
    
    public HttpDataReceiverMockup(final InputStream instream, int buffersize) {
        super();
        init(instream, buffersize);
    }

    public HttpDataReceiverMockup(final byte[] bytes) {
        this(bytes, BUFFER_SIZE);
    }

    public HttpDataReceiverMockup(final byte[] bytes, int buffersize) {
        this(new ByteArrayInputStream(bytes), buffersize);
    }

    public HttpDataReceiverMockup(final String s, final String charset, int buffersize) 
        throws UnsupportedEncodingException {
        this(s.getBytes(charset), buffersize);
    }
    
    public HttpDataReceiverMockup(final String s, final String charset) 
        throws UnsupportedEncodingException {
        this(s.getBytes(charset));
    
    }
    
    public boolean isDataAvailable(int timeout) throws IOException {
        return true;
    }
    
}
