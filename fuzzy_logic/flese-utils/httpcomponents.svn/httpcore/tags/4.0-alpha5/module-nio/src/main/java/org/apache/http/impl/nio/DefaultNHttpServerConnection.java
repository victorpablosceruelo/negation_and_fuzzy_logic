/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-nio/src/main/java/org/apache/http/impl/nio/DefaultNHttpServerConnection.java $
 * $Revision: 548056 $
 * $Date: 2007-06-17 18:11:01 +0200 (Sun, 17 Jun 2007) $
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

package org.apache.http.impl.nio;

import java.io.IOException;
import java.util.Iterator;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpRequestFactory;
import org.apache.http.HttpResponse;
import org.apache.http.message.BasicHeader;
import org.apache.http.message.BasicStatusLine;
import org.apache.http.message.BufferedHeader;
import org.apache.http.nio.NHttpServerConnection;
import org.apache.http.nio.NHttpServiceHandler;
import org.apache.http.impl.nio.codecs.HttpRequestParser;
import org.apache.http.nio.reactor.EventMask;
import org.apache.http.nio.reactor.IOSession;
import org.apache.http.nio.util.ByteBufferAllocator;
import org.apache.http.params.HttpParams;

public class DefaultNHttpServerConnection 
    extends NHttpConnectionBase implements NHttpServerConnection {

    private HttpRequestParser requestParser;
    
    public DefaultNHttpServerConnection(
            final IOSession session,
            final HttpRequestFactory requestFactory,
            final ByteBufferAllocator allocator,
            final HttpParams params) {
        super(session, allocator, params);
        if (requestFactory == null) {
            throw new IllegalArgumentException("Request factory may not be null");
        }
        this.requestParser = new HttpRequestParser(this.inbuf, requestFactory, params);
    }

    public void resetInput() {
        this.request = null;
        this.contentDecoder = null;
        this.requestParser.reset();
    }
    
    public void resetOutput() {
        this.response = null;
        this.contentEncoder = null;
    }
    
    public void consumeInput(final NHttpServiceHandler handler) {
        if (this.closed) {
            this.session.clearEvent(EventMask.READ);
            return;
        }
        try {
            if (this.request == null) {
                int bytesRead = this.requestParser.fillBuffer(this.session.channel());
                this.request = (HttpRequest) this.requestParser.parse(); 
                if (this.request != null) {
                    if (this.request instanceof HttpEntityEnclosingRequest) {
                        // Receive incoming entity
                        HttpEntity entity = prepareDecoder(this.request);
                        ((HttpEntityEnclosingRequest)this.request).setEntity(entity);
                    }
                    this.metrics.incrementRequestCount();
                    handler.requestReceived(this);
                    if (this.contentDecoder == null) {
                        // No request entity is expected
                        // Ready to receive a new request
                        resetInput();
                    }
                }
                if (bytesRead == -1) {
                    close();
                }
            }
            if (this.contentDecoder != null) {
                handler.inputReady(this, this.contentDecoder);
                if (this.contentDecoder.isCompleted()) {
                    // Request entity received
                    // Ready to receive a new request
                    resetInput();
                }
            }
        } catch (IOException ex) {
            handler.exception(this, ex);
        } catch (HttpException ex) {
            handler.exception(this, ex);
        } finally {
            // Finally set buffered input flag
            this.hasBufferedInput = this.inbuf.hasData();
        }
    }

    public void produceOutput(final NHttpServiceHandler handler) {
        try {
            if (this.outbuf.hasData()) {
                this.outbuf.flush(this.session.channel());
            }
            if (!this.outbuf.hasData()) {
                if (this.closed) {
                    this.session.close();
                    resetOutput();
                    return;
                } else {
                    if (this.contentEncoder != null) {
                        handler.outputReady(this, this.contentEncoder);
                        if (this.contentEncoder.isCompleted()) {
                            resetOutput();
                        }
                    }
                }
                
                if (this.contentEncoder == null && !this.outbuf.hasData()) {
                    if (this.closed) {
                        this.session.close();
                        return;
                    }
                    
                    this.session.clearEvent(EventMask.WRITE);
               
                    handler.responseReady(this);
                }
            }
        } catch (IOException ex) {
            handler.exception(this, ex);
        } finally {
            // Finally set the buffered output flag
            this.hasBufferedOutput = this.outbuf.hasData();
        }
    }
    
    public void submitResponse(final HttpResponse response) throws IOException, HttpException {
        if (response == null) {
            throw new IllegalArgumentException("HTTP response may not be null");
        }
        assertNotClosed();
        if (this.response != null) {
            throw new HttpException("Response already submitted");
        }
        this.lineBuffer.clear();
        BasicStatusLine.format(this.lineBuffer, response.getStatusLine());
        this.outbuf.writeLine(this.lineBuffer);
        for (Iterator it = response.headerIterator(); it.hasNext(); ) {
            Header header = (Header) it.next();
            if (header instanceof BufferedHeader) {
                // If the header is backed by a buffer, re-use the buffer
                this.outbuf.writeLine(((BufferedHeader)header).getBuffer());
            } else {
                this.lineBuffer.clear();
                BasicHeader.format(this.lineBuffer, header);
                this.outbuf.writeLine(this.lineBuffer);
            }
        }
        this.lineBuffer.clear();
        this.outbuf.writeLine(this.lineBuffer);

        if (response.getStatusLine().getStatusCode() >= 200) {
            this.metrics.incrementRequestCount();
            if (response.getEntity() != null) {
                this.response = response;
                prepareEncoder(response);
            }
        }

        this.session.setEvent(EventMask.WRITE);
    }

    public boolean isResponseSubmitted() {
        return this.response != null;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("[");
        if (isOpen()) {
            buffer.append(this.session.getRemoteAddress());
        } else {
            buffer.append("closed");
        }
        buffer.append("]");
        return buffer.toString();
    }
    
}
