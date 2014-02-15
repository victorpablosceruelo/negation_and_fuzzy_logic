/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-nio/src/main/java/org/apache/http/nio/impl/reactor/SessionInputBuffer.java $
 * $Revision: 474012 $
 * $Date: 2006-11-12 20:04:47 +0100 (Sun, 12 Nov 2006) $
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

package org.apache.http.nio.impl.reactor;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;

import org.apache.http.nio.util.ExpandableBuffer;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.CharArrayBuffer;

public class SessionInputBuffer extends ExpandableBuffer {
    
    private CharBuffer charbuffer = null;
    private Charset charset = null;
    private CharsetDecoder chardecoder = null;
    
    public SessionInputBuffer(int buffersize, int linebuffersize) {
        super(buffersize);
        this.charbuffer = CharBuffer.allocate(linebuffersize);
        this.charset = Charset.forName("US-ASCII");
        this.chardecoder = this.charset.newDecoder();
    }

    public void reset(final HttpParams params) {
        this.charset = Charset.forName(HttpProtocolParams.getHttpElementCharset(params)); 
        this.chardecoder = this.charset.newDecoder();
        clear();
    }

    public int fill(final ReadableByteChannel channel) throws IOException {
        if (channel == null) {
            throw new IllegalArgumentException("Channel may not be null");
        }
        setInputMode();
        if (!this.buffer.hasRemaining()) {
            expand();
        }
        int readNo = channel.read(this.buffer);
        return readNo;
    }
    
    public int read() {
        setOutputMode();
        return this.buffer.get() & 0xff;
    }
    
    public int read(final ByteBuffer dst, int maxLen) {
        if (dst == null) {
            return 0;
        }
        setOutputMode();
        int len = Math.min(dst.remaining(), maxLen);
        int chunk = Math.min(this.buffer.remaining(), len);
        for (int i = 0; i < chunk; i++) {
            dst.put(this.buffer.get());
        }
        return chunk;
    }
    
    public int read(final ByteBuffer dst) {
        if (dst == null) {
            return 0;
        }
        return read(dst, dst.remaining());
    }
    
    public boolean readLine(final CharArrayBuffer linebuffer, boolean endOfStream) {
        setOutputMode();
        // See if there is LF char present in the buffer
        int pos = -1;
        boolean hasLine = false;
        for (int i = this.buffer.position(); i < this.buffer.limit(); i++) {
            int b = this.buffer.get(i);
            if (b == HTTP.LF) {
                hasLine = true;
                pos = i + 1;
                break;
            }
        }
        if (!hasLine) {
            if (endOfStream && this.buffer.hasRemaining()) {
                // No more data. Get the rest
                pos = this.buffer.limit();
            } else {
                // Either no complete line present in the buffer 
                // or no more data is expected
                return false;
            }
        }
        int origLimit = this.buffer.limit();
        this.buffer.limit(pos);

        int len = this.buffer.limit() - this.buffer.position();
        // Ensure capacity of len assuming ASCII as the most likely charset
        linebuffer.ensureCapacity(len);
        
        this.chardecoder.reset();
        
        for (;;) {
            CoderResult result = this.chardecoder.decode(
                    this.buffer, 
                    this.charbuffer, 
                    true);
            if (result.isOverflow()) {
                this.charbuffer.flip();
                linebuffer.append(
                        this.charbuffer.array(), 
                        this.charbuffer.position(), 
                        this.charbuffer.remaining());
                this.charbuffer.clear();
            }
            if (result.isUnderflow()) {
                break;
            }
        }
        this.buffer.limit(origLimit);

        // flush the decoder
        this.chardecoder.flush(this.charbuffer);
        this.charbuffer.flip();
        // append the decoded content to the line buffer
        if (this.charbuffer.hasRemaining()) {
            linebuffer.append(
                    this.charbuffer.array(), 
                    this.charbuffer.position(), 
                    this.charbuffer.remaining());
        }
        
        // discard LF if found
        int l = linebuffer.length(); 
        if (l > 0) {
            if (linebuffer.charAt(l - 1) == HTTP.LF) {
                l--;
                linebuffer.setLength(l);
            }
            // discard CR if found
            if (l > 0) {
                if (linebuffer.charAt(l - 1) == HTTP.CR) {
                    l--;
                    linebuffer.setLength(l);
                }
            }
        }
        return true;
    }
    
    public String readLine(boolean endOfStream) {
        CharArrayBuffer charbuffer = new CharArrayBuffer(64);
        boolean found = readLine(charbuffer, endOfStream);
        if (found) {
            return charbuffer.toString();
        } else {
            return null;
        }
    }
    
}
