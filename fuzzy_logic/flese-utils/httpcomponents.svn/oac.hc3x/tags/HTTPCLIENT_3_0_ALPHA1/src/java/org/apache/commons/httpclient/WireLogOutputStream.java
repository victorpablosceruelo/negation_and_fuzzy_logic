/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/WireLogOutputStream.java,v 1.6 2004/04/18 23:51:35 jsdever Exp $
 * $Revision: 1.6 $
 * $Date: 2004-04-19 01:51:38 +0200 (Mon, 19 Apr 2004) $
 *
 * ====================================================================
 *
 *  Copyright 2002-2004 The Apache Software Foundation
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

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Logs all data written to the wire LOG.
 *
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * 
 * @since 2.0beta1
 */
class WireLogOutputStream extends FilterOutputStream {

    /** Original input stream. */
    private OutputStream out;    

    /**
     * Create an instance that wraps the specified output stream.
     * @param out The output stream.
     */
    public WireLogOutputStream(OutputStream out) {
        super(out);
        this.out = out;
    }
    
    /**
     * 
     * @see java.io.OutputStream#write(byte[], int, int)
     */
    public void write(byte[] b, int off, int len) throws IOException {
        this.out.write(b,  off,  len);
        Wire.output(b, off, len);
    }

    /**
     * 
     * @see java.io.OutputStream#write()
     */
    public void write(int b) throws IOException {
        this.out.write(b);
        Wire.output(b);
    }

    /**
     * 
     * @see java.io.OutputStream#write(byte[])
     */
    public void write(byte[] b) throws IOException {
        this.out.write(b);
        Wire.output(b);
    }
}
