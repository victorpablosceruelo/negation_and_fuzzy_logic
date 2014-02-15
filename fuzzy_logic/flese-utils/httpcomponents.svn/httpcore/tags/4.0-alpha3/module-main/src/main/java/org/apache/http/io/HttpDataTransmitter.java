/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/main/java/org/apache/http/io/HttpDataTransmitter.java $
 * $Revision: 473982 $
 * $Date: 2006-11-12 17:17:43 +0100 (Sun, 12 Nov 2006) $
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

package org.apache.http.io;

import java.io.IOException;

import org.apache.http.params.HttpParams;
import org.apache.http.util.CharArrayBuffer;

/**
 * Interface for sending data.
 * Unlike {@link java.io.OutputStream}, this interface is tailored
 * to the needs of the HTTP components.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 473982 $
 * 
 * @since 4.0
 */
public interface HttpDataTransmitter {

    void reset(HttpParams params);

    void write(byte[] b, int off, int len) throws IOException;
    
    void write(byte[] b) throws IOException;
    
    void write(int b) throws IOException;
    
    void writeLine(String s) throws IOException;
    
    void writeLine(CharArrayBuffer buffer) throws IOException;
    
    void flush() throws IOException;
    
}
