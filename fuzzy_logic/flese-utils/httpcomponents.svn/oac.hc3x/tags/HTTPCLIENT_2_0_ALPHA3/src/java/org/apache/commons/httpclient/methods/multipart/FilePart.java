/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/multipart/FilePart.java,v 1.12 2003/02/25 02:10:17 jsdever Exp $
 * $Revision: 1.12 $
 * $Date: 2003-02-25 03:10:18 +0100 (Tue, 25 Feb 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
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

package org.apache.commons.httpclient.methods.multipart;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import org.apache.commons.httpclient.HttpConstants;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * This class implements a part of a Multipart post object that
 * consists of a file.  
 *
 * @author <a href="mailto:mattalbright@yahoo.com">Matthew Albright</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:adrian@ephox.com">Adrian Sutton</a>
 * @author <a href="mailto:becke@u.washington.edu">Michael Becke</a>
 * @author <a href="mailto:mdiggory@latte.harvard.edu">Mark Diggory</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 *   
 * @since 2.0 
 *
 */
public class FilePart extends Part {

    /** Default content encoding of file attachments. */
    public static final String DEFAULT_CONTENT_TYPE = "application/octet-stream";

    /** Default charset of file attachments. */
    public static final String DEFAULT_CHARSET = HttpConstants.DEFAULT_CONTENT_CHARSET;

    /** Default transfer encoding of file attachments. */
    public static final String DEFAULT_TRANSFER_ENCODING = "binary";

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(FilePart.class);


    /** Attachment's file name */
    protected static final String FILE_NAME = "; filename=";

    /** Attachment's file name as a byte array */
    protected static final byte[] FILE_NAME_BYTES = 
      HttpConstants.getAsciiBytes(FILE_NAME);

    /** Name of the file part. */
    private String name;

    /** Source of the file part. */
    private PartSource source;
        
    /** Content type of the file part. */
    private String contentType;

    /** Content encoding of the file part. */
    private String charset;

    /**
     * FilePart Constructor.
     *
     * @param name the name for this part
     * @param partSource the source for this part
     * @param contentType the content type for this part
     * @param charset the charset encoding for this part
     */
    public FilePart(String name, PartSource partSource, String contentType, String charset) {
        LOG.trace("enter FilePart(String, PartSource, String, String)");
        if (name == null) {
            throw new IllegalArgumentException("Name may not be null");
        }
        this.name = name;
        if (partSource == null) {
            throw new IllegalArgumentException("Source may not be null");
        }
        if (partSource.getLength() < 0) {
            throw new IllegalArgumentException("Source length must be >= 0");
        }
        this.source = partSource;
        if (contentType != null) {
            this.contentType = contentType;
        } else {
            this.contentType = DEFAULT_CONTENT_TYPE;
        }
        this.charset = charset;
    }
        
    /**
     * FilePart Constructor.
     *
     * @param name the name for this part
     * @param partSource the source for this part
     */
    public FilePart(String name, PartSource partSource) {
        this(name, partSource, null, null);
    }

    /**
     * FilePart Constructor.
     *
     * @param name the name of the file part
     * @param file the file to post
     *
     * @throws FileNotFoundException if the <i>file</i> is not a normal
     * file or if it is not readable.
     */
    public FilePart(String name, File file) 
    throws FileNotFoundException {
        this(name, new FilePartSource(file), null, null);
    }

    /**
     * FilePart Constructor.
     *
     * @param name the name of the file part
     * @param file the file to post
     * @param contentType the content type for the file
     * @param charset the charset encoding of the file
     *
     * @throws FileNotFoundException if the <i>file</i> is not a normal
     * file or if it is not readable.
     */
    public FilePart(String name, File file, String contentType, String charset) 
    throws FileNotFoundException {
        this(name, new FilePartSource(file), contentType, charset);
    }

     /**
     * FilePart Constructor.
     *
     * @param name the name of the file part
     * @param fileName the file name 
     * @param file the file to post
     *
     * @throws FileNotFoundException if the <i>file</i> is not a normal
     * file or if it is not readable.
     */
    public FilePart(String name, String fileName, File file) 
    throws FileNotFoundException {
        this(name, new FilePartSource(fileName, file), null, null);
    }
    
     /**
     * FilePart Constructor.
     *
     * @param name the name of the file part
     * @param fileName the file name 
     * @param file the file to post
     * @param contentType the content type for the file
     * @param charset the charset encoding of the file
     *
     * @throws FileNotFoundException if the <i>file</i> is not a normal
     * file or if it is not readable.
     */
    public FilePart(String name, String fileName, File file, String contentType, String charset) 
    throws FileNotFoundException {
        this(name, new FilePartSource(fileName, file), contentType, charset);
    }
    
    /**
     * Return the name.
     * @return The name.
     * @see org.apache.commons.httpclient.methods.multipart.Part#getName()
     */
    public String getName() { 
        return this.name; 
    }

    /**
     * Return the content type of this part.
     * @return String The name.
     */
    public String getContentType() {
        return this.contentType;
    }

    /**
     * Return the character encoding of this part.
     * @return String The name.
     */
    public String getCharSet() {
        return this.charset;
    }

    /**
     * Return the transfer encoding of this part.
     * @return String The name.
     */

    public String getTransferEncoding() {
        return DEFAULT_TRANSFER_ENCODING;
    }
    
    /**
     * Write the disposition header to the output stream
     * @param out The output stream
     * @throws IOException If an IO problem occurs
     * @see Part#sendDispositionHeader(OutputStream)
     */
    protected void sendDispositionHeader(OutputStream out) 
    throws IOException {
        LOG.trace("enter sendDispositionHeader(OutputStream out)");
        super.sendDispositionHeader(out);
        String filename = this.source.getFileName();
        if (filename != null) {
            out.write(FILE_NAME_BYTES);
            out.write(QUOTE_BYTES);
            out.write(HttpConstants.getAsciiBytes(filename));
            out.write(QUOTE_BYTES);
        }
    }
    
    /**
     * Write the data in "source" to the specified stream.
     * @param out The output stream.
     * @throws IOException if an IO problem occurs.
     * @see org.apache.commons.httpclient.methods.multipart.Part#sendData(OutputStream)
     */
    protected void sendData(OutputStream out) throws IOException {
        LOG.trace("enter sendData(OutputStream out)");
        if (lengthOfData() == 0) {
            
            // this file contains no data, so there is nothing to send.
            // we don't want to create a zero length buffer as this will
            // cause an infinite loop when reading.
            LOG.debug("No data to send.");
            return;
        }
        
        byte[] tmp = new byte[4096];
        InputStream instream = source.createInputStream();

        int len;
        while ((len = instream.read(tmp)) >= 0) {
            out.write(tmp, 0, len);
        }
    }

    /**
     * Return the length of the data.
     * @return The length.
     * @throws IOException if an IO problem occurs
     * @see org.apache.commons.httpclient.methods.multipart.Part#lengthOfData()
     */    
    protected long lengthOfData() throws IOException {
        LOG.trace("enter lengthOfData()");
        return source.getLength();
    }    

}
