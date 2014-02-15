/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta3/module-nio/src/main/java/org/apache/http/nio/util/SimpleOutputBuffer.java $
 * $Revision: 613298 $
 * $Date: 2008-01-18 23:09:22 +0100 (Fri, 18 Jan 2008) $
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
package org.apache.http.nio.util;

import java.io.IOException;

import org.apache.http.nio.ContentEncoder;

public class SimpleOutputBuffer extends ExpandableBuffer implements ContentOutputBuffer {
    
    private boolean endOfStream;
    
    public SimpleOutputBuffer(int buffersize, final ByteBufferAllocator allocator) {
        super(buffersize, allocator);
        this.endOfStream = false;
    }

    public int produceContent(final ContentEncoder encoder) throws IOException {
        setOutputMode();
        int bytesWritten = encoder.write(this.buffer);
        if (!hasData() && this.endOfStream) {
            encoder.complete();
        }
        return bytesWritten;
    }
    
    public void write(final byte[] b, int off, int len) throws IOException {
        if (b == null) {
            return;
        }
        if (this.endOfStream) {
            return;
        }
        setInputMode();
        ensureCapacity(this.buffer.position() + len);
        this.buffer.put(b, off, len);
    }

    public void write(final byte[] b) throws IOException {
        if (b == null) {
            return;
        }
        if (this.endOfStream) {
            return;
        }
        write(b, 0, b.length);
    }

    public void write(int b) throws IOException {
        if (this.endOfStream) {
            return;
        }
        setInputMode();
        ensureCapacity(this.capacity() + 1);
        this.buffer.put((byte)b);
    }
    
    public void reset() {
        super.clear();
        this.endOfStream = false;
    }
    
    public void flush() {
    }

    public void writeCompleted() {
        this.endOfStream = true;
    }
    
    public void shutdown() {
        this.endOfStream = true;
    }
    
}
