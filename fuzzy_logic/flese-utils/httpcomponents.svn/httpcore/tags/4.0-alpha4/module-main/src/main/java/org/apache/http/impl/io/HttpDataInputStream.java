/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-main/src/main/java/org/apache/http/impl/io/HttpDataInputStream.java $
 * $Revision: 503192 $
 * $Date: 2007-02-03 12:55:49 +0100 (Sat, 03 Feb 2007) $
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

package org.apache.http.impl.io;

import java.io.IOException;
import java.io.InputStream;

import org.apache.http.io.HttpDataReceiver;

/**
 * A stream for reading from a {@link HttpDataReceiver HttpDataReceiver}.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 503192 $
 * 
 * @since 4.0
 */
public class HttpDataInputStream extends InputStream {
    
    private final HttpDataReceiver datareceiver;
    
    private boolean closed = false;
    
    public HttpDataInputStream(final HttpDataReceiver datareceiver) {
        super();
        if (datareceiver == null) {
            throw new IllegalArgumentException("HTTP data receiver may not be null");
        }
        this.datareceiver = datareceiver;
    }
    
    public int available() throws IOException {
        if (!this.closed && this.datareceiver.isDataAvailable(10)) {
            return 1;
        } else {
            return 0;
        }
    }
    
    public void close() throws IOException {
        this.closed = true;
    }

    public int read() throws IOException {
        if (this.closed) {
            return -1;
        } else {
            return this.datareceiver.read();
        }
    }
    
    public int read(final byte[] b, int off, int len) throws IOException {
        if (this.closed) {
            return -1;
        } else {
            return this.datareceiver.read(b, off, len);
        }
    }
    
}
