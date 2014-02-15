/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-nio/src/main/java/org/apache/http/nio/ContentDecoder.java $
 * $Revision: 473994 $
 * $Date: 2006-11-12 18:18:52 +0100 (Sun, 12 Nov 2006) $
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

package org.apache.http.nio;

import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * Abstract HTTP content decoder. HTTP content decoders can be used
 * to read entity content from the underlying channel in small
 * chunks and apply the required coding transformation.
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public interface ContentDecoder {

    /**
     * Reads a portion of content from the underlying channel
     * 
     * @param dst The buffer into which entity content is to be transferred
     * @return The number of bytes read, possibly zero, or -1 if the 
     * channel has reached end-of-stream
     * @throws IOException if I/O error occurs while reading content
     */
    int read(ByteBuffer dst) throws IOException;
    
    /**
     * Returns <tt>true</tt> if the entity has been received in its
     * entirety.
     * 
     * @return <tt>true</tt> if all the content has been consumed, 
     * <tt>false</tt> otherwise.
     */
    boolean isCompleted();
    
}
