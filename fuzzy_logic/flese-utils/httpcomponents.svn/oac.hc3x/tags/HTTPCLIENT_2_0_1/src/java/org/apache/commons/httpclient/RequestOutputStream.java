/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/RequestOutputStream.java,v 1.21.2.1 2004/02/22 18:21:13 olegk Exp $
 * $Revision: 1.21.2.1 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.IOException;
import java.io.OutputStream;


/**
 * <p>
 * {@link OutputStream} wrapper supporting the chunked transfer encoding.
 * </p>
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Sean C. Sullivan
 * @author <a href="mailto:dion@apache.org">dIon Gillard</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @version $Revision: 1.21.2.1 $ $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
 * @deprecated Use new ChunkedOutputStream(HttpConnecion#getRequestOutputStream());
 *
 */
public class RequestOutputStream
    extends OutputStream {

    // ----------------------------------------------------------- Constructors

    /**
     * Construct an output stream wrapping the given stream.
     * The stream will not use chunking.
     *
     * @param stream wrapped output stream. Must be non-null.
     * 
     * @deprecated Use ChunkedOutputStream;
     */
    public RequestOutputStream(OutputStream stream) {
        this(stream, false);
    }

    /**
     * Construct an output stream wrapping the given stream.
     *
     * @param stream wrapped output stream. Must be non-null.
     * @param useChunking when <code>true</code>, the chunked transfer encoding
     *                    will be used
     * 
     * @deprecated Use ChunkedOutputStream;
     */
    public RequestOutputStream(OutputStream stream, boolean useChunking) {
        if (stream == null) {
            throw new NullPointerException("stream parameter is null");
        }
        this.stream = stream;
        this.useChunking = useChunking;
    }

    // ------------------------------------------------------- Static Variables

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(RequestOutputStream.class);

    // ----------------------------------------------------- Instance Variables

    /** Has this stream been closed? */
    private boolean closed = false;

    /** The underlying output stream to which we will write data */
    private OutputStream stream = null;

    /** True if chunking is allowed */
    private boolean useChunking = false;

    /** <tt>"\r\n"</tt>, as bytes. */
    private static final byte CRLF[] = new byte[] {(byte) 13, (byte) 10};

    /** End chunk */
    private static final byte ENDCHUNK[] = CRLF;

    /** 0 */
    private static final byte ZERO[] = new byte[] {(byte) '0'};

    /** 1 */
    private static final byte ONE[] = new byte[] {(byte) '1'};

    // ------------------------------------------------------------- Properties


    /**
     * Use chunking flag setter.
     *
     * @param useChunking true if chunking is to be used, false otherwise
     * 
     * @deprecated Use ChunkedOutputStream;
     */
    public void setUseChunking(boolean useChunking) {
        this.useChunking = useChunking;
    }


    /**
     * Use chunking flag getter.
     *
     * @return true if chunking is to be used, false otherwise
     * 
     * @deprecated Use ChunkedOutputStream;
     */
    public boolean isUseChunking() {
        return useChunking;
    }

    // --------------------------------------------------------- Public Methods

    /**
     * Writes a <code>String</code> to the client, without a carriage return
     * line feed (CRLF) character at the end.
     *
     * @param s the <code>String</code> to send to the client. Must be non-null.
     * @throws IOException if an input or output exception occurred
     * 
     * @deprecated Use ChunkedOutputStream;
     */
    public void print(String s) throws IOException {
        LOG.trace("enter RequestOutputStream.print(String)");

        if (s == null) {
            s = "null";
        }
        int len = s.length();
        for (int i = 0; i < len; i++) {
            write(s.charAt(i));
        }
    }

    /**
     * Writes a carriage return-line feed (CRLF) to the client.
     *
     * @throws IOException   if an input or output exception occurred
     * 
     * @deprecated Use ChunkedOutputStream;
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
     * 
     * @deprecated Use ChunkedOutputStream;
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
     * @throws IOException if an input/output error occurs
     * 
     * @deprecated Use ChunkedOutputStream;
     */
    public void write(int b) throws IOException {

        //FIXME: If using chunking, the chunks are ONE byte long!
        if (useChunking) {
            stream.write(ONE, 0, ONE.length);
            stream.write(CRLF, 0, CRLF.length);
            stream.write(b);
            stream.write(ENDCHUNK, 0, ENDCHUNK.length);
        } else {
            stream.write(b);
        }
    }

    /**
     * Write the specified byte array.
     *
     * @param b the byte array to write out
     * @param off the offset within <code>b</code> to start writing from
     * @param len the length of data within <code>b</code> to write
     * @throws IOException when errors occur writing output
     * 
     * @deprecated Use ChunkedOutputStream;
     */
    public void write(byte[] b, int off, int len) throws IOException {
        LOG.trace("enter RequestOutputStream.write(byte[], int, int)");

        if (useChunking) {
            byte chunkHeader[] = HttpConstants.getBytes(Integer.toHexString(len) + "\r\n");
            stream.write(chunkHeader, 0, chunkHeader.length);
            stream.write(b, off, len);
            stream.write(ENDCHUNK, 0, ENDCHUNK.length);
        } else {
            stream.write(b, off, len);
        }
    }

    /**
     * Close this output stream, causing any buffered data to be flushed and
     * any further output data to throw an IOException.
     *
     * @throws IOException if an error occurs closing the stream
     * 
     * @deprecated Use ChunkedOutputStream;
     */
    public void close() throws IOException {
        LOG.trace("enter RequestOutputStream.close()");

        if (!closed) {
            try {
                if (useChunking) {
                    // Write the final chunk.
                    stream.write(ZERO, 0, ZERO.length);
                    stream.write(CRLF, 0, CRLF.length);
                    stream.write(ENDCHUNK, 0, ENDCHUNK.length);
                }
            } catch (IOException ioe) {
                LOG.debug("Unexpected exception caught when closing output "
                    + " stream", ioe);
                throw ioe;
            } finally {
                // regardless of what happens, mark the stream as closed.
                // if there are errors closing it, there's not much we can do
                // about it
                closed = true;
                super.close();
            }
        }

    }

}
