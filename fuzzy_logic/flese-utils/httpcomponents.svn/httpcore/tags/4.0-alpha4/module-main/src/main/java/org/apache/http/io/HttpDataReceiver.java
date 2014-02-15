/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-main/src/main/java/org/apache/http/io/HttpDataReceiver.java $
 * $Revision: 496070 $
 * $Date: 2007-01-14 13:18:34 +0100 (Sun, 14 Jan 2007) $
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

package org.apache.http.io;

import java.io.IOException;

import org.apache.http.params.HttpParams;
import org.apache.http.util.CharArrayBuffer;

/**
 * Interface for receiving data.
 * Unlike {@link java.io.InputStream}, this interface is tailored
 * to the needs of the HTTP components.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 496070 $
 * 
 * @since 4.0
 */
public interface HttpDataReceiver {
    
    void reset(HttpParams params);

    int read(byte[] b, int off, int len) throws IOException; 
    
    int read(byte[] b) throws IOException; 
    
    int read() throws IOException; 
    
    int readLine(CharArrayBuffer buffer) throws IOException;
    
    String readLine() throws IOException;
    
    boolean isDataAvailable(int timeout) throws IOException; 

}
