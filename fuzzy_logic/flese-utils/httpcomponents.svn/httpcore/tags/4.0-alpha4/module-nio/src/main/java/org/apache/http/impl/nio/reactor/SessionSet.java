/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-nio/src/main/java/org/apache/http/impl/nio/reactor/SessionSet.java $
 * $Revision: 503301 $
 * $Date: 2007-02-03 21:39:44 +0100 (Sat, 03 Feb 2007) $
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

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.apache.http.nio.reactor.IOSession;

public class SessionSet {
    
    private final Set set;
    
    public SessionSet() {
        super();
        this.set = new HashSet();
    }

    public void add(final IOSession session) {
        if (session == null) {
            return;
        }
        this.set.add(session);
    }

    public boolean remove(final IOSession session) {
        if (session == null) {
            return false;
        }
        return this.set.remove(session);
    }

    public void clear() {
        this.set.clear();
    }

    public boolean isEmpty() {
        return this.set.isEmpty();
    }
    
    public Iterator iterator() {
        return this.set.iterator();
    }
    
}
