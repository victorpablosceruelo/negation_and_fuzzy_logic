/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/main/java/org/apache/http/io/HttpDataInputStream.java $
 * $Revision: 376961 $
 * $Date: 2006-02-11 11:32:50 +0100 (Sat, 11 Feb 2006) $
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
import java.io.InputStream;

/**
 * A stream for reading from a {@link HttpDataReceiver HttpDataReceiver}.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 376961 $
 * 
 * @since 4.0
 */
public class HttpDataInputStream extends InputStream {
    
    private final HttpDataReceiver datareceiver;
    
    private boolean closed = false;
    
    public HttpDataInputStream(final HttpDataReceiver datareceiver) {
        super();
        if (datareceiver == null) {
            throw new IllegalArgumentException("HTTP data receiver may not be null");
        }
        this.datareceiver = datareceiver;
    }
    
    public int available() throws IOException {
        if (!this.closed && this.datareceiver.isDataAvailable(10)) {
            return 1;
        } else {
            return 0;
        }
    }
    
    public void close() throws IOException {
        this.closed = true;
    }

    public int read() throws IOException {
        if (this.closed) {
            return -1;
        } else {
            return this.datareceiver.read();
        }
    }
    
    public int read(final byte[] b, int off, int len) throws IOException {
        if (this.closed) {
            return -1;
        } else {
            return this.datareceiver.read(b, off, len);
        }
    }
    
}
