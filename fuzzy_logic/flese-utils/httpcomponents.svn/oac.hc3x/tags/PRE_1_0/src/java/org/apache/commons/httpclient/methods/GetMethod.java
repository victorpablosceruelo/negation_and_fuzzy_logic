/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/GetMethod.java,v 1.2 2001/05/21 19:59:58 rwaldhoff Exp $
 * $Revision: 1.2 $
 * $Date: 2001-05-21 21:59:58 +0200 (Mon, 21 May 2001) $
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

package org.apache.commons.httpclient.methods;

import java.io.*;
import java.util.*;
import java.net.URLEncoder;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.State;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpMethodBase;

/**
 * GET Method.
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Sung-Gu Park
 */
public class GetMethod
    extends HttpMethodBase {


    // -------------------------------------------------------------- Constants


    /**
     * Temporary directory.
     */
    public static final String TEMP_DIR = "temp/";

    // ----------------------------------------------------------- Constructors


    /**
     * Method constructor.
     */
    public GetMethod() {
        name = "GET";
        setFollowRedirects(true);
    }


    /**
     * Method constructor.
     */
    public GetMethod(String path) {
        super(path);
        name = "GET";
        setFollowRedirects(true);
    }


    /**
     * Method constructor.
     */
    public GetMethod(String path, String tempDir) {
        this(path);
        useDisk = true;
        setTempDir(tempDir);
        setFollowRedirects(true);
    }


    /**
     * Method constructor.
     */
    public GetMethod(String path, boolean useDisk, String tempDir) {
        this(path);
        setUseDisk(useDisk);
        if (useDisk)
            setTempDir(tempDir);
        setFollowRedirects(true);
    }


    /**
     * Method constructor.
     */
    public GetMethod(String path, String tempDir, String tempFile) {
        this(path);
        useDisk = true;
        setTempDir(tempDir);
        setTempFile(tempFile);
        setFollowRedirects(true);
    }

    /**
     * Method constructor.
     */
    public GetMethod(String path, boolean useDisk, String tempDir,
                     String tempFile) {
        this(path);
        setUseDisk(useDisk);
        if (useDisk) {
            setTempDir(tempDir);
            setTempFile(tempFile);
        }
        setFollowRedirects(true);
    }


    /**
     * Method constructor.
     */
    public GetMethod(String path, File fileData) {
        this(path);
        useDisk = true;
        this.fileData = fileData;
        setFollowRedirects(true);
    }


    // ----------------------------------------------------- Instance Variables


    /**
     * By default, the get method will buffer read data to the disk.
     */
    protected boolean useDisk = true;


    /**
     * If we're not using the HD, we're using a memory byte buffer.
     */
    protected byte[] memoryData;


    /**
     * File which contains the buffered data.
     */
    protected File fileData;


    /**
     * Temporary directory to use.
     */
    protected String tempDir = TEMP_DIR;


    /**
     * Temporary file to use.
     */
    protected String tempFile = null;


    // ------------------------------------------------------------- Properties


    /**
     * Use disk setter.
     *
     * @param useDisk New value of useDisk
     */
    public void setUseDisk(boolean useDisk) {
        checkNotUsed();
        this.useDisk = useDisk;
    }


    /**
     * Use disk getter.
     *
     * @param boolean useDisk value
     */
    public boolean getUseDisk() {
        return useDisk;
    }

    /**
     * Temporary directory setter.
     *
     * @param tempDir New value of tempDir
     */
    public void setTempDir(String tempDir) {
        checkNotUsed();
        this.tempDir = tempDir;
    }


    /**
     * Temporary directory getter.
     */
    public String getTempDir() {
        return tempDir;
    }


    /**
     * Temporary file setter.
     *
     * @param tempFile New value of tempFile
     */
    public void setTempFile(String tempFile) {
        checkNotUsed();
        this.tempFile = tempFile;
    }


    /**
     * Temporary file getter.
     */
    public String getTempFile() {
        return tempFile;
    }


    /**
     * File data getter.
     */
    public File getFileData() {
        return fileData;
    }


    /**
     * File data setter.
     */
    public void setFileData(File fileData) {
        checkNotUsed();
        this.fileData = fileData;
    }


    // --------------------------------------------------------- Public Methods

   // override recycle to reset redirects default
   public void recycle() {
        super.recycle();
        setFollowRedirects(true);
    }


    /**
     * Get read data.
     */
    public InputStream getData()
        throws IOException {

        checkUsed();

        if (useDisk) {
            return new FileInputStream(fileData);
        } else {
            if (memoryData == null)
                throw new IOException("Data not found");
            return new ByteArrayInputStream(memoryData);
        }

    }


    /**
     * Get read data as a String.
     */
    public String getDataAsString()
        throws IOException {

        checkUsed();

        if (useDisk) {
            InputStream is = new FileInputStream(fileData);
            byte[] buffer = new byte[4096];
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            int nb = 0;
            while (true) {
                nb = is.read(buffer);
                if (nb == -1)
                    break;
                os.write(buffer, 0, nb);
            }
            is.close();
            return os.toString();
        } else {
            if (memoryData != null)
                return new String(memoryData);
            else
                return "";
        }

    }


    // ----------------------------------------------------- HttpMethod Methods


    /**
     * Generate the query body.
     *
     * @return String query
     */
    public String generateQuery() {
        return null;
    }


    /**
     * Parse response.
     *
     * @param is Input stream
     */
    public void parseResponse(InputStream is)
        throws IOException {

        OutputStream out = null;

        if (useDisk) {

            if (fileData == null) {

                // Create a temporary file on the HD
                File dir = new File(tempDir);
                dir.mkdirs();

                String tempFileName = null;
                if (tempFile == null) {
                    String encodedPath = URLEncoder.encode(getPath());
                    int length = encodedPath.length();
                    if (length > 240) {
                        encodedPath =
                            encodedPath.substring(length - 200, length);
                    }
                    tempFileName = System.currentTimeMillis() + "-"
                        + encodedPath + ".tmp";
                } else {
                    tempFileName = tempFile;
                }

                fileData = new File(tempDir, tempFileName);

            }

            out = new FileOutputStream(fileData);

        } else {
            out = new ByteArrayOutputStream();
        }

        byte[] buffer = new byte[4096];
        int nb = 0;
        while (true) {
            nb = is.read(buffer);
            if (nb == -1)
                break;
            if (out == null)
                throw new IOException("Unable to buffer data");
            out.write(buffer, 0, nb);
        }

        if (!useDisk)
            memoryData = ((ByteArrayOutputStream) out).toByteArray();

        out.close();

    }


    /**
     * Return true if the method needs a content-length header in the request.
     *
     * @return true if a content-length header will be expected by the server
     */
    public boolean needContentLength() {
        return false;
    }


}
