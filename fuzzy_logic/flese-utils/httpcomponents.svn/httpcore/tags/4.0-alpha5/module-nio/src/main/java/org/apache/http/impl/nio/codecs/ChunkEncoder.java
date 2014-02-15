/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-nio/src/main/java/org/apache/http/impl/nio/codecs/ChunkEncoder.java $
 * $Revision: 544208 $
 * $Date: 2007-06-04 20:48:11 +0200 (Mon, 04 Jun 2007) $
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

package org.apache.http.impl.nio.codecs;

import java.io.IOException;
import java.nio.ByteBuffer;

import org.apache.http.impl.nio.reactor.SessionOutputBuffer;
import org.apache.http.util.CharArrayBuffer;

public class ChunkEncoder extends AbstractContentEncoder {
    
    private final SessionOutputBuffer outbuf;
    private final CharArrayBuffer lineBuffer;
    
    public ChunkEncoder(final SessionOutputBuffer outbuf) {
        super();
        if (outbuf == null) {
            throw new IllegalArgumentException("Session output buffer may not be null");
        }
        this.outbuf = outbuf;
        this.lineBuffer = new CharArrayBuffer(16);
    }

    public int write(final ByteBuffer src) throws IOException {
        if (src == null) {
            return 0;
        }
        assertNotCompleted();
        int chunk = src.remaining();
        if (chunk == 0) {
            return 0;
        }
        this.lineBuffer.clear();
        this.lineBuffer.append(Integer.toHexString(chunk));
        this.outbuf.writeLine(this.lineBuffer);
        this.outbuf.write(src);
        this.lineBuffer.clear();
        this.outbuf.writeLine(this.lineBuffer);
        return chunk;
    }

    public void complete() throws IOException {
        assertNotCompleted();
        this.lineBuffer.clear();
        this.lineBuffer.append("0");
        this.outbuf.writeLine(this.lineBuffer);
        this.lineBuffer.clear();
        this.outbuf.writeLine(this.lineBuffer);
        this.completed = true;
    }
    
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("[chunk-coded; completed: ");
        buffer.append(this.completed);
        buffer.append("]");
        return buffer.toString();
    }
    
}
