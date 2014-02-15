/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-nio/src/main/java/org/apache/http/impl/nio/reactor/ChannelEntry.java $
 * $Revision: 517167 $
 * $Date: 2007-03-12 11:50:32 +0100 (Mon, 12 Mar 2007) $
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

package org.apache.http.impl.nio.reactor;

import java.nio.channels.SocketChannel;

public class ChannelEntry {

    private final SocketChannel channel;
    private final SessionRequestImpl sessionRequest;
    
    public ChannelEntry(final SocketChannel channel, final SessionRequestImpl sessionRequest) {
        super();
        if (channel == null) {
            throw new IllegalArgumentException("Socket channel may not be null");
        }
        this.channel = channel;
        this.sessionRequest = sessionRequest;
    }

    public ChannelEntry(final SocketChannel channel) {
        this(channel, null);
    }

    public SessionRequestImpl getSessionRequest() {
        return this.sessionRequest;
    }

    public Object getAttachment() {
        if (this.sessionRequest != null) {
            return this.sessionRequest.getAttachment();
        } else {
            return null;
        }
    }

    public SocketChannel getChannel() {
        return this.channel;
    }
    
}
