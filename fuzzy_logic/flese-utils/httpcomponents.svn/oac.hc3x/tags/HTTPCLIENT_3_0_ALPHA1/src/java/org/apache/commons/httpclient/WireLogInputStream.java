/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/WireLogInputStream.java,v 1.14 2004/04/18 23:51:35 jsdever Exp $
 * $Revision: 1.14 $
 * $Date: 2004-04-19 01:51:38 +0200 (Mon, 19 Apr 2004) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2004 The Apache Software Foundation
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

package org.apache.commons.httpclient;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Logs all data read to the wire LOG.
 *
 * @author Ortwin Glück
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * 
 * @since 2.0
 */
class WireLogInputStream extends FilterInputStream {
     
    /** Original input stream. */
    private InputStream in;    

    /**
     * Create an instance that wraps the specified input stream.
     * @param in The input stream.
     */
    public WireLogInputStream(InputStream in) {
        super(in);
        this.in = in;
    }
    /**
     * 
     * @see java.io.InputStream#read(byte[], int, int)
     */
    public int read(byte[] b, int off, int len) throws IOException {
        int l = this.in.read(b,  off,  len);
        if (l > 0) {
            Wire.input(b, off, l);
        }
        return l;
    }

    /**
     * 
     * @see java.io.InputStream#read()
     */
    public int read() throws IOException {
        int l = this.in.read();
        if (l > 0) { 
            Wire.input(l);
        }
        return l;
    }

    /**
     * 
     * @see java.io.InputStream#read(byte[])
     */
    public int read(byte[] b) throws IOException {
        int l = this.in.read(b);
        if (l > 0) {
            Wire.input(b, 0, l);
        }
        return l;
    }
}
