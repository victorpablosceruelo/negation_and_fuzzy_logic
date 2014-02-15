/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta3/module-nio/src/main/java/org/apache/http/nio/reactor/SessionInputBuffer.java $
 * $Revision: 594116 $
 * $Date: 2007-11-12 14:35:43 +0100 (Mon, 12 Nov 2007) $
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

package org.apache.http.nio.reactor;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.CharacterCodingException;

import org.apache.http.util.CharArrayBuffer;

/**
 * Session input buffer for non-blocking connections.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @since 4.0
 */
public interface SessionInputBuffer {
    
    boolean hasData();
    
    int length();
    
    int fill(ReadableByteChannel channel) 
        throws IOException;
    
    int read();
    
    int read(ByteBuffer dst, int maxLen);
    
    int read(ByteBuffer dst);
    
    int read(WritableByteChannel dst, int maxLen) throws IOException;
    
    int read(WritableByteChannel dst) throws IOException;
    
    boolean readLine(CharArrayBuffer linebuffer, boolean endOfStream) 
        throws CharacterCodingException;
    
    String readLine(boolean endOfStream) 
        throws CharacterCodingException;
    
}
