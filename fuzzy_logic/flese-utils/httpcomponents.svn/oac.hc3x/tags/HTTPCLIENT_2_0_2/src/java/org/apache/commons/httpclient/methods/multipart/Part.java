/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/multipart/Part.java,v 1.10.2.2 2004/02/22 18:21:15 olegk Exp $
 * $Revision: 1.10.2.2 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient.methods.multipart;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.commons.httpclient.HttpConstants;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Abstract class for one Part of a multipart post object.
 *
 * @author <a href="mailto:mattalbright@yahoo.com">Matthew Albright</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:adrian@ephox.com">Adrian Sutton</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 *
 * @since 2.0
 */
public abstract class Part {

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(Part.class);

    //TODO: Make this configurable
    
    /** The boundary */
    protected static final String BOUNDARY = "----------------314159265358979323846";
    
    /** The boundary as a byte array */
    protected static final byte[] BOUNDARY_BYTES = HttpConstants.getAsciiBytes(BOUNDARY);
    
    /** Carriage return/linefeed */
    protected static final String CRLF = "\r\n";
    
    /** Carriage return/linefeed as a byte array */
    protected static final byte[] CRLF_BYTES = HttpConstants.getAsciiBytes(CRLF);
    
    /** Content dispostion characters */
    protected static final String QUOTE = "\"";
    
    /** Content dispostion as a byte array */
    protected static final byte[] QUOTE_BYTES = 
      HttpConstants.getAsciiBytes(QUOTE);

    /** Extra characters */
    protected static final String EXTRA = "--";
    
    /** Extra characters as a byte array */
    protected static final byte[] EXTRA_BYTES = 
      HttpConstants.getAsciiBytes(EXTRA);
    
    /** Content dispostion characters */
    protected static final String CONTENT_DISPOSITION = "Content-Disposition: form-data; name=";
    
    /** Content dispostion as a byte array */
    protected static final byte[] CONTENT_DISPOSITION_BYTES = 
      HttpConstants.getAsciiBytes(CONTENT_DISPOSITION);

    /** Content type header */
    protected static final String CONTENT_TYPE = "Content-Type: ";

    /** Content type header as a byte array */
    protected static final byte[] CONTENT_TYPE_BYTES = 
      HttpConstants.getAsciiBytes(CONTENT_TYPE);

    /** Content charset */
    protected static final String CHARSET = "; charset=";

    /** Content charset as a byte array */
    protected static final byte[] CHARSET_BYTES = 
      HttpConstants.getAsciiBytes(CHARSET);

    /** Content type header */
    protected static final String CONTENT_TRANSFER_ENCODING = "Content-Transfer-Encoding: ";

    /** Content type header as a byte array */
    protected static final byte[] CONTENT_TRANSFER_ENCODING_BYTES = 
      HttpConstants.getAsciiBytes(CONTENT_TRANSFER_ENCODING);

    /**
     * Return the boundary string.
     * @return the boundary string
     */
    public static String getBoundary() {
        return BOUNDARY;
    }
    
    /**
     * Return the name of this part.
     * @return The name.
     */
    public abstract String getName();
    
    /**
     * Returns the content type of this part.
     * @return the content type, or <code>null</code> to exclude the content type header
     */
    public abstract String getContentType();

    /**
     * Return the character encoding of this part.
     * @return the character encoding, or <code>null</code> to exclude the character 
     * encoding header
     */
    public abstract String getCharSet();

    /**
     * Return the transfer encoding of this part.
     * @return the transfer encoding, or <code>null</code> to exclude the transfer encoding header
     */
    public abstract String getTransferEncoding();

    /**
     * Write the start to the specified output stream
     * @param out The output stream
     * @throws IOException If an IO problem occurs.
     */
    protected void sendStart(OutputStream out) throws IOException {
        LOG.trace("enter sendStart(OutputStream out)");
        out.write(EXTRA_BYTES);
        out.write(BOUNDARY_BYTES);
        out.write(CRLF_BYTES);
    }
    
    /**
     * Write the content disposition header to the specified output stream
     * 
     * @param out The output stream
     * @throws IOException If an IO problem occurs.
     */
    protected void sendDispositionHeader(OutputStream out) throws IOException {
        LOG.trace("enter sendDispositionHeader(OutputStream out)");
        out.write(CONTENT_DISPOSITION_BYTES);
        out.write(QUOTE_BYTES);
        out.write(HttpConstants.getAsciiBytes(getName()));
        out.write(QUOTE_BYTES);
    }
    
    /**
     * Write the content type header to the specified output stream
     * @param out The output stream
     * @throws IOException If an IO problem occurs.
     */
     protected void sendContentTypeHeader(OutputStream out) throws IOException {
        LOG.trace("enter sendContentTypeHeader(OutputStream out)");
        String contentType = getContentType();
        if (contentType != null) {
            out.write(CRLF_BYTES);
            out.write(CONTENT_TYPE_BYTES);
            out.write(HttpConstants.getAsciiBytes(contentType));
            String charSet = getCharSet();
            if (charSet != null) {
                out.write(CHARSET_BYTES);
                out.write(HttpConstants.getAsciiBytes(charSet));
            }
        }
    }

    /**
     * Write the content transfer encoding header to the specified 
     * output stream
     * 
     * @param out The output stream
     * @throws IOException If an IO problem occurs.
     */
     protected void sendTransferEncodingHeader(OutputStream out) throws IOException {
        LOG.trace("enter sendTransferEncodingHeader(OutputStream out)");
        String transferEncoding = getTransferEncoding();
        if (transferEncoding != null) {
            out.write(CRLF_BYTES);
            out.write(CONTENT_TRANSFER_ENCODING_BYTES);
            out.write(HttpConstants.getAsciiBytes(transferEncoding));
        }
    }

    /**
     * Write the end of the header to the output stream
     * @param out The output stream
     * @throws IOException If an IO problem occurs.
     */
    protected void sendEndOfHeader(OutputStream out) throws IOException {
        LOG.trace("enter sendEndOfHeader(OutputStream out)");
        out.write(CRLF_BYTES);
        out.write(CRLF_BYTES);
    }
    
    /**
     * Write the data to the specified output stream
     * @param out The output stream
     * @throws IOException If an IO problem occurs.
     */
    protected abstract void sendData(OutputStream out) throws IOException;
    
    /**
     * Return the length of the main content
     * 
     * @return long The length.
     * @throws IOException If an IO problem occurs
     */
    protected abstract long lengthOfData() throws IOException;
    
    /**
     * Write the end data to the output stream.
     * @param out The output stream
     * @throws IOException If an IO problem occurs.
     */
    protected void sendEnd(OutputStream out) throws IOException {
        LOG.trace("enter sendEnd(OutputStream out)");
        out.write(CRLF_BYTES);
    }
    
    /**
     * Write all the data to the output stream.
     * If you override this method make sure to override 
     * #length() as well
     * 
     * @param out The output stream
     * @throws IOException If an IO problem occurs.
     */
    public void send(OutputStream out) throws IOException {
        LOG.trace("enter send(OutputStream out)");
        sendStart(out);
        sendDispositionHeader(out);
        sendContentTypeHeader(out);
        sendTransferEncodingHeader(out);
        sendEndOfHeader(out);
        sendData(out);
        sendEnd(out);
    }


    /**
     * Return the full length of all the data.
     * If you override this method make sure to override 
     * #send(OutputStream) as well
     * 
     * @return long The length.
     * @throws IOException If an IO problem occurs
     */
    public long length() throws IOException {
        LOG.trace("enter length()");
        ByteArrayOutputStream overhead = new ByteArrayOutputStream();
        sendStart(overhead);
        sendDispositionHeader(overhead);
        sendContentTypeHeader(overhead);
        sendTransferEncodingHeader(overhead);
        sendEndOfHeader(overhead);
        sendEnd(overhead);
        return overhead.size() + lengthOfData();
    }

    /**
     * Return a string representation of this object.
     * @return A string representation of this object.
     * @see java.lang.Object#toString()
     */    
    public String toString() {
        return this.getName();
    }

    /**
     * Write all parts and the last boundary to the specified output stream
     * 
     * @param out The output stream
     * @param parts The array of parts to be sent
     * 
     * @throws IOException If an IO problem occurs.
     */
    public static void sendParts(OutputStream out, final Part[] parts)
    throws IOException {
        LOG.trace("enter sendParts(OutputStream out, Parts[])");
        if (parts == null) {
            throw new IllegalArgumentException("Parts may not be null"); 
        }
        for (int i = 0; i < parts.length; i++) {
            parts[i].send(out);
        }
        out.write(EXTRA_BYTES);
        out.write(BOUNDARY_BYTES);
        out.write(EXTRA_BYTES);
        out.write(CRLF_BYTES);
    }

    /**
     * Return the total sum of all parts and that of the last boundary
     * 
     * @param parts The array of parts
     * 
     * @return the total length
     * 
     * @throws IOException If an IO problem occurs.
     */
    public static long getLengthOfParts(final Part[] parts)
    throws IOException {
        LOG.trace("getLengthOfParts(Parts[])");
        if (parts == null) {
            throw new IllegalArgumentException("Parts may not be null"); 
        }
        long total = 0;
        for (int i = 0; i < parts.length; i++) {
            total += parts[i].length();
        }
        total += EXTRA_BYTES.length;
        total += BOUNDARY_BYTES.length;
        total += EXTRA_BYTES.length;
        total += CRLF_BYTES.length;
        return total;
    }        
}
