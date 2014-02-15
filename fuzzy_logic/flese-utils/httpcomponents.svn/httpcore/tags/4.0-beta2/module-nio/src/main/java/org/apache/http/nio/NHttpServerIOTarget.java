/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta2/module-nio/src/main/java/org/apache/http/nio/NHttpServerIOTarget.java $
 * $Revision: 591287 $
 * $Date: 2007-11-02 11:01:45 +0100 (Fri, 02 Nov 2007) $
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

package org.apache.http.nio;

import org.apache.http.nio.reactor.IOEventDispatch;

/**
 * Extended version of the {@link NHttpServerConnection} used by {@link IOEventDispatch}
 * implementations to inform server-side connection objects of I/O events.
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public interface NHttpServerIOTarget extends NHttpServerConnection {
    
    void consumeInput(NHttpServiceHandler handler);
    
    void produceOutput(NHttpServiceHandler handler);    

}
