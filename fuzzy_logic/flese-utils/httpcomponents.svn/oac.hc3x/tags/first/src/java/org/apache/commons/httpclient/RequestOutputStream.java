/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/RequestOutputStream.java,v 1.1 2001/04/25 18:42:52 remm Exp $
 * $Revision: 1.1 $
 * $Date: 2001-04-25 20:42:48 +0200 (Wed, 25 Apr 2001) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Tomcat", and "Apache Software
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


import java.io.OutputStream;
import java.io.IOException;
import java.util.Hashtable;


/**
 * Socket output stream wrapper.
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @version $Revision: 1.1 $ $Date: 2001-04-25 20:42:48 +0200 (Wed, 25 Apr 2001) $
 */

public class RequestOutputStream
    extends OutputStream {


    // ----------------------------------------------------------- Constructors


    /**
     * Construct an output stream associated with the specified headers.
     *
     * @param stream Wrapped input stream
     */
    public RequestOutputStream(OutputStream stream) {

        super();

        this.stream = stream;

    }


    // ----------------------------------------------------- Instance Variables


    /**
     * Stream interceptors.
     */
    protected StreamInterceptor interceptor = null;


    /**
     * Has this stream been closed?
     */
    protected boolean closed = false;


    /**
     * The underlying input stream from which we should read data.
     */
    protected OutputStream stream = null;


    /**
     * True if chunking is allowed.
     */
    private boolean useChunking = false;


    /**
     * True if printing a chunk.
     */
    private boolean writingChunk = false;


    /**
     * End chunk.
     */
    private byte endChunk[] = "\r\n".getBytes();


    /**
     * CRLF.
     */
    private byte crlf[] = "\r\n".getBytes();


    /**
     * 0.
     */
    private byte zero[] = "0".getBytes();


    /**
     * 1.
     */
    private byte one[] = "1".getBytes();


    // ------------------------------------------------------------- Properties


    /**
     * Use chunking flag setter.
     */
    public void setUseChunking(boolean useChunking) {
        this.useChunking = useChunking;
    }


    /**
     * Use chunking flag getter.
     */
    public boolean isUseChunking() {
        return useChunking;
    }


    /**
     * Get stream interceptor.
     */
    public void setInterceptor(StreamInterceptor interceptor) {
        this.interceptor = interceptor;
    }


    // --------------------------------------------------------- Public Methods


    /**
     * Writes a <code>String</code> to the client,
     * without a carriage return-line feed (CRLF)
     * character at the end.
     *
     * @param s         the <code>String</code to send to the client
     * @exception IOException   if an input or output exception occurred
     */
    public void print(String s) throws IOException {
        if (s == null)
                s = "null";
        int len = s.length();
        for (int i = 0; i < len; i++) {
            write(s.charAt(i));
        }
    }


    /**
     * Writes a carriage return-line feed (CRLF)
     * to the client.
     *
     * @exception IOException   if an input or output exception occurred
     */
    public void println() throws IOException {
        print("\r\n");
    }



    /**
     * Writes a <code>String</code> to the client,
     * followed by a carriage return-line feed (CRLF).
     *
     * @param s         the </code>String</code> to write to the client
     * @exception IOException   if an input or output exception occurred
     */
    public void println(String s) throws IOException {
        print(s);
        println();
    }


    // -------------------------------------------- ServletOutputStream Methods


    /**
     * Write the specified byte to our output stream.
     *
     * @param b The byte to be written
     *
     * @exception IOException if an input/output error occurs
     */
    public void write(int b)
        throws IOException {

        // Invoke interceptor
        if (interceptor != null) {
            interceptor.bytesWrite(null, b, 1);
        }

        if (useChunking) {
            stream.write(one, 0, one.length);
            stream.write(crlf, 0, crlf.length);
            stream.write(b);
            stream.write(endChunk, 0, endChunk.length);
        } else {
            stream.write(b);
        }
    }


    /**
     * Write the specified byte array.
     */
    public void write(byte[] b, int off, int len)
        throws IOException {

        // Invoke interceptor
        if (interceptor != null) {
            interceptor.bytesWrite(b, off, len);
        }

        if (useChunking) {
            byte chunkHeader[] =
                (Integer.toHexString(len) + "\r\n").getBytes();
            stream.write(chunkHeader, 0, chunkHeader.length);
            stream.write(b, off, len);
            stream.write(endChunk, 0, endChunk.length);
        } else {
            stream.write(b, off, len);
        }
    }


    /**
     * Close this output stream, causing any buffered data to be flushed and
     * any further output data to throw an IOException.
     */
    public void close() throws IOException {

        if (useChunking) {
            // Write the final chunk.
            stream.write(zero, 0, zero.length);
            stream.write(crlf, 0, crlf.length);
            stream.write(endChunk, 0, endChunk.length);
        }
        super.close();

    }


}
