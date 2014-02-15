/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-nio/src/main/java/org/apache/http/impl/nio/codecs/IdentityDecoder.java $
 * $Revision: 531601 $
 * $Date: 2007-04-23 23:12:15 +0200 (Mon, 23 Apr 2007) $
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

import org.apache.http.impl.nio.reactor.SessionInputBuffer;
import org.apache.http.nio.FileContentDecoder;

public class IdentityDecoder extends AbstractContentDecoder 
        implements FileContentDecoder {
    
    public IdentityDecoder(final ReadableByteChannel channel, final SessionInputBuffer buffer) {
        super(channel, buffer);
    }

    public int read(final ByteBuffer dst) throws IOException {
        if (dst == null) {
            throw new IllegalArgumentException("Byte buffer may not be null");
        }
        if (this.completed) {
            return -1;
        }
        
        int bytesRead;
        if (this.buffer.hasData()) {
            bytesRead = this.buffer.read(dst);
        } else {
            bytesRead = this.channel.read(dst);
        }
        if (bytesRead == -1) {
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
        
        long bytesRead;
        if (this.buffer.hasData()) {
            ByteBuffer tmpDst = ByteBuffer.allocate((int)count);
            this.buffer.read(tmpDst);
            tmpDst.flip();
            bytesRead = fileChannel.write(tmpDst);
        } else {
            bytesRead = fileChannel.transferFrom(this.channel, position, count);
        }
        if (bytesRead == 0) {
            this.completed = true;
        }
        return bytesRead;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("[identity; completed: ");
        buffer.append(this.completed);
        buffer.append("]");
        return buffer.toString();
    }
    
}
