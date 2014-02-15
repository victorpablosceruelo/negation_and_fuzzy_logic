/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/ContentLengthInputStream.java,v 1.4 2003/01/23 22:47:45 jsdever Exp $
 * $Revision: 1.4 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "HttpClient", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient;

import java.io.*;

/**
 * Cuts the wrapped InputStream off after a specified number of bytes.
 *
 * @author Ortwin Glück
 * @author Eric Johnson
 * @since 2.0
 */

public class ContentLengthInputStream extends FilterInputStream {
    private int contentLength;
    private int pos = 0;

    private boolean closed = false;

    /**
     * Creates a new length limited stream
     *
     * @param in The stream to wrap
     * @param contentLength The maximum number of bytes that can be read from
     * the stream. Subsequent read operations will return -1.
     */
    public ContentLengthInputStream(InputStream in, int contentLength) {
        super(in);
        this.contentLength = contentLength;
    }

    /**
     * Reads until the end of the known length of content.
     *
     * <p>Does not close the underlying socket input, but instead leaves it
     * primed to parse the next response.</p>
     */
    public void close() throws IOException {
        if (!closed) {
            try {
                ChunkedInputStream.exhaustInputStream(this);
            } finally {
                // close after above so that we don't throw an exception trying
                // to read after closed!
                closed = true;
            }
        }
    }

    public int read() throws java.io.IOException {
        if (closed)
            throw new IOException("Attempted read from closed stream.");

        if (pos >= contentLength)
            return -1;
        pos++;
        return super.read();
    }

    /**
     * Does standard {@link InputStream#read(byte[], int, int)} behavior, but
     * also notifies the watcher when the contents have been consumed.
     *
     * @param b     The byte array to fill.
     * @param off   Start filling at this position.
     * @param len   The number of bytes to attempt to read.
     * @return The number of bytes read, or -1 if the end of content has been
     *  reached.
     *
     * @throws java.io.IOException Should an error occur on the wrapped stream.
     */
    public int read(byte[] b, int off, int len) throws java.io.IOException {
        if (closed)
            throw new IOException("Attempted read from closed stream.");

        if (pos >= contentLength)
            return -1;

        if (pos + len > contentLength) {
            len = contentLength - pos;
        }
        int count = super.read(b, off, len);
        pos += count;
        return count;
    }


    public int read(byte[] b) throws java.io.IOException {
        return read(b, 0, b.length);
    }

}
