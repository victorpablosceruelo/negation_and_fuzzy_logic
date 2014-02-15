/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/multipart/FilePart.java,v 1.9 2003/01/25 00:59:11 jsdever Exp $
 * $Revision: 1.9 $
 * $Date: 2003-01-25 01:59:11 +0100 (Sat, 25 Jan 2003) $
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
 *   
 * @since 2.0 
 *
 */
public class FilePart extends Part {

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(FilePart.class);

    //TODO: make this configurable
    static int MAX_BUFF_SIZE = 1 * 1024 * 1024;  // 1 MiBs
    
    /** Name of the file part. */
    private String name;

    /** Source of the file part. */
    private PartSource source;
        
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
        this(name, new FilePartSource(file));
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
        this(name, new FilePartSource(fileName, file));
    }
    
    /**
     * FilePart Constructor.
     *
     * @param name the name for this part
     * @param partSource the source for this part
     */
    public FilePart(String name, PartSource partSource) {

        if (partSource.getLength() < 0) {
            throw new IllegalArgumentException("fileLength must be >= 0");
        }

        this.name = name;
        this.source = partSource;

    }
        
    protected void sendHeader(OutputStream out) 
    throws IOException {
        log.trace("enter sendHeader(OutputStream out)");
        super.sendHeader(out);
        sendFilename(out);
        sendContentType(out);
    }
    
    protected void sendFilename(OutputStream out) 
    throws IOException {
        log.trace("enter sendFilename(OutputStream out)");
        String filename = "; filename=\"" + source.getFileName() + "\"";
        out.write(HttpConstants.getBytes(filename));
    }

    
    protected void sendContentType(OutputStream out) 
    throws IOException {
        log.trace("enter sendContentType(OutputStream out)");
        out.write(CRLF_bytes);
        out.write(HttpConstants.getBytes("Content-Type: application/octet-stream"));
    }    

    public String getName() { 
        return name; 
    }
    
    protected void sendData(OutputStream out) 
    throws IOException {
        log.trace("enter sendData(OutputStream out)");
        
        byte[] buff;
        
        if ( lengthOfData() == 0 ) {
            
            // this file contains no data, so there is nothing to send.
            // we don't want to create a zero length buffer as this will
            // cause an infinite loop when reading.
            log.debug("No data to send.");
            return;
            
        } else if (lengthOfData() > MAX_BUFF_SIZE) {
            buff = new byte[MAX_BUFF_SIZE];
        } else {
            buff = new byte[(new Long(lengthOfData())).intValue()];
        }
        
        InputStream is = source.createInputStream();

        int len;
        while ((len = is.read(buff)) != -1) {
            out.write(buff, 0, len);
        }

    }
    
    protected long lengthOfData() 
    throws IOException {
        return source.getLength();
    }    

}

