/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-nio/src/main/java/org/apache/http/nio/impl/codecs/LengthDelimitedEncoder.java $
 * $Revision: 466406 $
 * $Date: 2006-10-21 16:49:07 +0200 (Sat, 21 Oct 2006) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2006 The Apache Software Foundation
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.http.nio.impl.codecs;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.WritableByteChannel;

public class LengthDelimitedEncoder extends AbstractContentEncoder {
    
    private final WritableByteChannel channel;
    private final long contentLength;
    
    private long len;

    public LengthDelimitedEncoder(final WritableByteChannel channel, long contentLength) {
        super();
        if (channel == null) {
            throw new IllegalArgumentException("Channel may not be null");
        }
        if (contentLength < 0) {
            throw new IllegalArgumentException("Content length may not be negative");
        }
        this.channel = channel;
        this.contentLength = contentLength;
        this.len = 0;
    }

    public int write(final ByteBuffer src) throws IOException {
        if (src == null) {
            return 0;
        }
        assertNotCompleted();
        int lenRemaining = (int) (this.contentLength - this.len);

        int bytesWritten;
        if (src.remaining() > lenRemaining) {
            int oldLimit = src.limit();
            int newLimit = oldLimit - (src.remaining() - lenRemaining);
            src.limit(newLimit);
            bytesWritten = this.channel.write(src);
            src.limit(oldLimit);
        } else {
            bytesWritten = this.channel.write(src);
        }
        this.len += bytesWritten;
        if (this.len >= this.contentLength) {
            this.completed = true;
        }
        return bytesWritten;
    }
    
}
