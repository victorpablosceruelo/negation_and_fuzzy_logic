/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta2/module-nio/src/main/java/org/apache/http/nio/NHttpMessageParser.java $
 * $Revision: 562518 $
 * $Date: 2007-08-03 18:38:19 +0200 (Fri, 03 Aug 2007) $
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

import java.io.IOException;
import java.nio.channels.ReadableByteChannel;

import org.apache.http.HttpException;
import org.apache.http.HttpMessage;

/**
 * Abstract HTTP message parser for non-blocking connections.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 562518 $
 * 
 * @since 4.0
 */
public interface NHttpMessageParser {
    
    void reset();
    
    int fillBuffer(ReadableByteChannel channel) 
        throws IOException;    

    HttpMessage parse()
        throws IOException, HttpException;

}
