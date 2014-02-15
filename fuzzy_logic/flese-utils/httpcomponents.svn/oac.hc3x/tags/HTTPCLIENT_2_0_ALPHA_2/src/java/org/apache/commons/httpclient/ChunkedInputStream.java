/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/ChunkedInputStream.java,v 1.9 2003/01/23 22:47:44 jsdever Exp $
 * $Revision: 1.9 $
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
 * <p>Transparently coalesces chunks of a HTTP stream that uses Transfer-Encoding
 * chunked.</p>
 *
 * <p>Note that this class NEVER closes the underlying stream, even when close gets
 * called.  Instead, it will read until the "end" of its chunking on close, which
 * allows for the seamless invocation of subsequent HTTP 1.1 calls, while not
 * requiring the client to remember to read the entire contents of the response.</p>
 *
 * @see ResponseInputStream
 *
 * @author Ortwin Glück
 * @author Sean C. Sullivan
 * @author Martin Elwin
 * @author Eric Johnson
 *
 * @since 2.0
 *
 */

public class ChunkedInputStream extends InputStream {
    private InputStream in;
    private int chunkSize, pos;
    private boolean eof = false;
    private boolean closed = false;
    private HttpMethod method;

    /**
     *
     *
     * @param in must be non-null
     * @param method must be non-null
     *
     * @throws java.io.IOException
     * @throws java.lang.NullPointerException
     *
     */
    public ChunkedInputStream(InputStream in, HttpMethod method) throws IOException {
      if (null == in) {
        throw new NullPointerException("InputStream parameter");
      }
      if (null == method) {
        throw new NullPointerException("HttpMethod parameter");
      }
        this.in = in;
        this.method = method;
        this.chunkSize = getChunkSizeFromInputStream(in);
        if (chunkSize == 0) {
            eof = true;
            parseFooters();
        }
        this.pos = 0;
    }

    /**
     * Returns all the data in a chunked stream in coalesced form. A chunk is
     * followed by a CRLF. The method returns -1 as soon as a chunksize of 0 is
     * detected.
     * Footers are read automcatically at the end of the stream and can be obtained
     * with the getFooters() method.
     *
     * @return -1 of the end of the stream has been reached or the next data byte
     * @throws IOException
     */
    public int read() throws IOException {

        if (closed)
            throw new IOException("Attempted read from closed stream.");
        if (eof) return -1;
        if (pos >= chunkSize) {
            nextChunk();
            if (eof) return -1;
        }
        pos++;
        return in.read();
    }

    public int read(byte[] b, int off, int len) throws java.io.IOException {

        if (closed)
            throw new IOException("Attempted read from closed stream.");

        if (eof) return -1;
        if (pos >= chunkSize) {
            nextChunk();
            if (eof) return -1;
        }
        len = Math.min(len, chunkSize - pos);
        int count = in.read(b, off, len);
        pos += count;
        return count;
    }

    public int read(byte[] b) throws java.io.IOException {
        return read(b, 0, b.length);
    }

    private void nextChunk() throws IOException {
        int cr = in.read();
        int lf = in.read();
        if ((cr != '\r') ||
            (lf != '\n')) throw new IOException("CRLF expected at end of chunk: "+cr+"/"+lf);
        chunkSize = getChunkSizeFromInputStream(in);
        pos = 0;
        if (chunkSize == 0) {
            eof = true;
            parseFooters();
        }
    }

    /**
     * Expects the stream to start with a chunksize in hex with optional comments
     * after a semicolon. The line must end with a CRLF:
     * "a3; some comment\r\n"
     * Positions the stream at the start of the next line.
     *
     * @return the chunk size as integer
     *
     * @throws IOException when the chunk size could not be parsed
     * @throws java.lang.RuntimeException
     *
     *
     */
    private static int getChunkSizeFromInputStream(final InputStream in) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int state = 0; //0: normal, 1: \r was scanned, 2: inside quoted string, -1: end
        while (state != -1) {
        int b = in.read();
            if (b == -1) throw new IOException("chunked stream ended unexpectedly");
            switch (state) {
                case 0:
                    switch (b) {
                        case '\r':
                            state = 1;
                    break;
                        case '\"':
                            state = 2;
                            /* fall through */
                        default:
                            baos.write(b);
                    }
                    break;

                case 1:
                    if (b == '\n') {
                        state = -1;
                    } else {
                        // this was not CRLF
                        throw new IOException("Protocol violation: Unexpected single newline character in chunk size");
                    }
                    break;

                case 2:
                    switch (b) {
                        case '\\':
                            b = in.read();
                            baos.write(b);
                            break;
                        case '\"':
                            state = 0;
                            /* fall through */
                        default:
                            baos.write(b);
                    }
                    break;
                default: throw new RuntimeException("assertion failed");
            }
        }

        //parse data
        String dataString = HttpConstants.getString(baos.toByteArray());
        int separator = dataString.indexOf(';');
        dataString = (separator > 0)
            ? dataString.substring(0, separator).trim()
            : dataString.trim();

        int result;
        try {
            result = Integer.parseInt(dataString.trim(), 16);
        } catch(NumberFormatException e) {
            throw new IOException("Bad chunk size: "+dataString);
        }
        return result;
    }

    /**
     * Stores the footers into map of Headers
     */
    private void parseFooters() throws IOException {
        String line = readLine();
        while ((line != null) && (!line.equals(""))) {
            int colonPos = line.indexOf(':');
            if (colonPos != -1) {
                String key = line.substring(0, colonPos).trim();
                String val = line.substring(colonPos+1).trim();
                Header footer = new Header(key, val);
                method.addResponseFooter(footer);
            }
            line = readLine();
        }
    }

    private String readLine() throws IOException {
        StringBuffer buf = new StringBuffer();
        for(;;) {
            int ch = in.read();
            if(ch < 0) {
                if(buf.length() == 0) {
                    return null;
                } else {
                    break;
                }
            } else if (ch == '\r') {
                continue;
            } else if (ch == '\n') {
                break;
            }
            buf.append((char)ch);
        }
        return (buf.toString());
    }

    /**
     * Upon close, this reads the remainder of the chunked message,
     * leaving the underlying socket at a position to start reading the
     * next response without scanning.
     */
    public void close() throws IOException {
        if (!closed) {
            try {
                if (!eof) {
                    exhaustInputStream(this);
                }
            }
            finally {
                eof = true;
                closed = true;
            }
        }
    }

    /**
     * Exhaust an input stream, reading until EOF has been encountered.
     *
     * <p>Note that this function is intended as a non-public utility.
     * This is a little weird, but it seemed silly to make a utility
     * class for this one function, so instead it is just static and
     * shared that way.</p>
     *
     * @param inStream The {@link InputStream} to exhaust.
     */
    static void exhaustInputStream(InputStream inStream) throws IOException {
        // read and discard the remainder of the message
        byte buffer[] = new byte[1024];
        while ( inStream.read(buffer) >= 0) {
            ;
        }
    }
}
