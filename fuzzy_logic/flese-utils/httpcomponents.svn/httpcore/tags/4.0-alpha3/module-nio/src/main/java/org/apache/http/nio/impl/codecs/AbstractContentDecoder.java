/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-nio/src/main/java/org/apache/http/nio/impl/codecs/AbstractContentDecoder.java $
 * $Revision: 473997 $
 * $Date: 2006-11-12 18:27:39 +0100 (Sun, 12 Nov 2006) $
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

import java.nio.channels.ReadableByteChannel;

import org.apache.http.nio.ContentDecoder;
import org.apache.http.nio.impl.reactor.SessionInputBuffer;

public abstract class AbstractContentDecoder implements ContentDecoder {
    
    protected final ReadableByteChannel channel;
    protected final SessionInputBuffer buffer;
    
    protected boolean completed;
    
    public AbstractContentDecoder(final ReadableByteChannel channel, final SessionInputBuffer buffer) {
        super();
        if (channel == null) {
            throw new IllegalArgumentException("Channel may not be null");
        }
        if (buffer == null) {
            throw new IllegalArgumentException("Session input buffer may not be null");
        }
        this.buffer = buffer;
        this.channel = channel;
    }

    public boolean isCompleted() {
        return this.completed;
    }
    
}
