/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha6/module-nio/src/main/java/org/apache/http/impl/nio/codecs/LengthDelimitedDecoder.java $
 * $Revision: 560528 $
 * $Date: 2007-07-28 13:34:17 +0200 (Sat, 28 Jul 2007) $
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
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;

import org.apache.http.impl.io.HttpTransportMetricsImpl;
import org.apache.http.nio.FileContentDecoder;
import org.apache.http.nio.reactor.SessionInputBuffer;

/**
 * Content-Length delimited decoder implementation.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * @author Andrea Selva
 *
 * @version $Revision: 560528 $
 * 
 * @since 4.0
 */
public class LengthDelimitedDecoder extends AbstractContentDecoder 
        implements FileContentDecoder {
    
    private final long contentLength;
    
    private long len;

    public LengthDelimitedDecoder(
            final ReadableByteChannel channel, 
            final SessionInputBuffer buffer,
            final HttpTransportMetricsImpl metrics,
            long contentLength) {
        super(channel, buffer, metrics);
        if (contentLength < 0) {
            throw new IllegalArgumentException("Content length may not be negative");
        }
        this.contentLength = contentLength;
    }

    public int read(final ByteBuffer dst) throws IOException {
        if (dst == null) {
            throw new IllegalArgumentException("Byte buffer may not be null");
        }
        if (this.completed) {
            return -1;
        }
        int lenRemaining = (int) (this.contentLength - this.len);
        
        int bytesRead;
        if (this.buffer.hasData()) {
            int maxLen = Math.min(lenRemaining, this.buffer.length());
            bytesRead = this.buffer.read(dst, maxLen);
        } else {
            if (dst.remaining() > lenRemaining) {
                int oldLimit = dst.limit();
                int newLimit = oldLimit - (dst.remaining() - lenRemaining);
                dst.limit(newLimit);
                bytesRead = this.channel.read(dst);
                dst.limit(oldLimit);
            } else {
                bytesRead = this.channel.read(dst);
            }
            if (bytesRead > 0) {
                this.metrics.incrementBytesTransferred(bytesRead);
            }
        }
        if (bytesRead == -1) {
            this.completed = true;
            return -1;
        }
        this.len += bytesRead;
        if (this.len >= this.contentLength) {
            this.completed = true;
        }
        return bytesRead;
    }
    
    public long read(final FileChannel fileChannel, long position, long count) throws IOException {
        if (fileChannel == null) {
            return 0;
        }
        if (this.completed) {
            return 0;
        }
        
        int lenRemaining = (int) (this.contentLength - this.len);
        
        long bytesRead;
        if (this.buffer.hasData()) {
            int maxLen = Math.min(lenRemaining, this.buffer.length());
            ByteBuffer tmpDst = ByteBuffer.allocate(maxLen);
            this.buffer.read(tmpDst, maxLen);
            bytesRead = fileChannel.write(tmpDst);
        } else {
            if (count > lenRemaining) {
                bytesRead = fileChannel.transferFrom(this.channel, position, lenRemaining);
            } else {
                bytesRead = fileChannel.transferFrom(this.channel, position, count);
            }
            if (bytesRead > 0) {
                this.metrics.incrementBytesTransferred(bytesRead);
            }
        }
        if (bytesRead == 0) {
            this.completed = true;
            return 0;
        }
        this.len += bytesRead;
        if (this.len >= this.contentLength) {
            this.completed = true;
        }
        return bytesRead;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("[content length: ");
        buffer.append(this.contentLength);
        buffer.append("; pos: ");
        buffer.append(this.len);
        buffer.append("; completed: ");
        buffer.append(this.completed);
        buffer.append("]");
        return buffer.toString();
    }
}
