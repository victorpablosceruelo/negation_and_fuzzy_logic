/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-nio/src/main/java/org/apache/http/impl/nio/reactor/SessionQueue.java $
 * $Revision: 503269 $
 * $Date: 2007-02-03 18:58:25 +0100 (Sat, 03 Feb 2007) $
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

import java.util.LinkedList;

import org.apache.http.nio.reactor.IOSession;

public class SessionQueue {
    
    private final LinkedList list;
    
    public SessionQueue() {
        super();
        this.list = new LinkedList();
    }

    public synchronized void push(final IOSession session) {
        if (session == null) {
            return;
        }
        this.list.addLast(session);
    }

    public synchronized IOSession pop() {
        if (!this.list.isEmpty()) {
            return (IOSession) this.list.removeFirst();
        } else {
            return null;
        }
    }
    
}
