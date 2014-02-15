/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/GetMethod.java,v 1.21 2003/01/23 22:48:06 jsdever Exp $
 * $Revision: 1.21 $
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
package org.apache.commons.httpclient.methods;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URLEncoder;

import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethodBase;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * GET Method. Implements an HTTP GET request.
 * 
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Sung-Gu Park
 * @author Sean C. Sullivan
 * @since 1.0
 */
public class GetMethod extends HttpMethodBase {
    //~ Static variables/initializers ··········································

    // -------------------------------------------------------------- Constants

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(GetMethod.class);

    /** Temporary directory. */
    private static final String TEMP_DIR = "temp/";


    // ----------------------------------------------------- Instance Variables

    /** File which contains the buffered data. */
    private File fileData;

    /** Temporary directory to use. */
    private String tempDir = TEMP_DIR;

    /** Temporary file to use. */
    private String tempFile = null;

    /** By default, the get method will buffer read data to the memory. */
    private boolean useDisk = false;

    //~ Constructors ···························································

    // ----------------------------------------------------------- Constructors

    /**
     * No-arg constructor.
     * 
     * @since 1.0
     */
    public GetMethod() {
        setFollowRedirects(true);
    }

    /**
     * Constructor specifying a URI.
     *
     * @param uri either an absolute or relative URI
     * 
     * @since 1.0
     */
    public GetMethod(String uri) {
        super(uri);
        log.trace("enter GetMethod(String)");
        setFollowRedirects(true);
    }

    /**
     * Constructor.
     * 
     * @param path the path to request
     * @param tempDir the directory in which to store temporary files
     * 
     * @since 1.0
     */
    public GetMethod(String path, String tempDir) {
        super(path);
        log.trace("enter GetMethod(String, String)");
        setUseDisk(true);
        setTempDir(tempDir);
        setFollowRedirects(true);
    }

    /**
     * Constructor.
     * 
     * @param path the path to request
     * @param tempDir the directory in which to store temporary files
     * @param tempFile the file (under tempDir) to buffer contents to
     * 
     * @since 1.0
     */
    public GetMethod(String path, String tempDir, String tempFile) {
        super(path);
        log.trace("enter GetMethod(String, String, String)");
        setUseDisk(true);
        setTempDir(tempDir);
        setTempFile(tempFile);
        setFollowRedirects(true);
    }

    /**
     * Constructor.
     * 
     * @param path the path to request
     * @param fileData the file to buffer contents to
     * 
     * @since 1.0
     */
    public GetMethod(String path, File fileData) {
        this(path);
        log.trace("enter GetMethod(String, File)");
        useDisk = true;
        this.fileData = fileData;
        setFollowRedirects(true);
    }

    //~ Methods ································································

    /**
     * File data setter.
     * 
     * @param fileData the file to buffer data to
     * 
     * @since 1.0
     */
    public void setFileData(File fileData) {
        checkNotUsed();
        this.fileData = fileData;
    }

    /**
     * File data getter.
     * 
     * @return the file being used for buffering data
     * 
     * @since 1.0
     */
    public File getFileData() {
        return fileData;
    }

    // --------------------------------------------------------- Public Methods

    /**
     * Returns <tt>"GET"</tt>.
     * 
     * @return <tt>"GET"</tt>
     * 
     * @since 2.0
     */
    public String getName() {
        return "GET";
    }

    /**
     * Return my response body, if any, as a byte array. Otherwise return
     * <tt>null</tt>.
     * 
     * @return the response body as a byte array
     * 
     * @since 2.0
     */
    public byte[] getResponseBody() {
        log.trace("enter GetMethod.getResponseBody()");

        checkUsed();
        return super.getResponseBody();
    }

    /**
     * Return my response body, if any, as an {@link InputStream}. Otherwise
     * return <tt>null</tt>.
     * 
     * @return a stream to read the response from
     * 
     * @throws IOException when there is an error reading the response
     * 
     * @since 2.0
     */
    public InputStream getResponseBodyAsStream() throws IOException {
        log.trace("enter GetMethod.getResponseBodyAsStream()");

        checkUsed();
        return super.getResponseBodyAsStream();
    }


    /**
     * Temporary directory setter.
     * 
     * @param tempDir New value of tempDir
     * 
     * @since 1.0
     */
    public void setTempDir(String tempDir) {
        checkNotUsed();
        this.tempDir = tempDir;
        setUseDisk(true);
    }

    /**
     * Temporary directory getter.
     * 
     * @return the current temporary directory
     * 
     * @since 1.0
     */
    public String getTempDir() {
        return tempDir;
    }

    /**
     * Temporary file setter.
     * 
     * @param tempFile New value of tempFile
     * 
     * @since 1.0
     */
    public void setTempFile(String tempFile) {
        checkNotUsed();
        this.tempFile = tempFile;
    }

    /**
     * Temporary file getter.
     * 
     * @return the current temporary file
     * 
     * @since 1.0
     */
    public String getTempFile() {
        return tempFile;
    }

    // ------------------------------------------------------------- Properties

    /**
     * Buffer the response in a file or not. The default is false.
     * 
     * @param useDisk If true the entire response will be buffered in a
     *        temporary file.
     * 
     * @since 1.0
     */
    public void setUseDisk(boolean useDisk) {
        checkNotUsed();
        this.useDisk = useDisk;
    }

    /**
     * Tells if the response will be buffered in a file.
     * 
     * @return true if the response will be buffered
     * 
     * @since 1.0
     */
    public boolean getUseDisk() {
        return useDisk;
    }

    /**
     * Override recycle to reset redirects default.
     * 
     * @since 1.0
     */
    public void recycle() {
        log.trace("enter GetMethod.recycle()");

        super.recycle();
        this.fileData = null;
        setFollowRedirects(true);
    }

    // ----------------------------------------------------- HttpMethod Methods

    /**
     * Overrides method in {@link HttpMethodBase} to write data to the
     * appropriate buffer.
     * 
     * @param state the shared http state
     * @param conn the connection to read data from
     * 
     * @throws IOException when there are problems reading from the connection
     * 
     * @since 2.0
     */
    protected void readResponseBody(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        log.trace("enter GetMethod.readResponseBody(HttpState, HttpConnection)");

        super.readResponseBody(state, conn);

        
        OutputStream out = null;
        if (useDisk) {
            out = new FileOutputStream(createTempFile());
            InputStream in = getResponseBodyAsStream();
            byte[] buffer = new byte[10000];
            int len ;
            while ((len = in.read(buffer)) > 0) {
                out.write(buffer, 0, len);
            }
            in.close();
            out.close();
            setResponseStream(new FileInputStream(createTempFile()));
        }
    }

    /**
     * Returns the file buffer, creating it if necessary. The created file is
     * deleted when the VM exits.
     * @return Temporary file to hold the data buffer.
     */
    private File createTempFile() {
        if (fileData == null) {
            // Create a temporary file on the HD
            File dir = new File(tempDir);
            dir.deleteOnExit();
            dir.mkdirs();
            String tempFileName = null;
            if (tempFile == null) {
                String encodedPath = URLEncoder.encode(getPath());
                int length = encodedPath.length();
                if (length > 200) {
                    encodedPath =
                        encodedPath.substring(length - 190, length);
                }
                tempFileName = System.currentTimeMillis() + "-"
                    + encodedPath + ".tmp";
            } else {
                tempFileName = tempFile;
            }
            fileData = new File(tempDir, tempFileName);
 
            fileData = new File(tempDir, tempFileName);
            fileData.deleteOnExit();
        }
        return fileData;
    }
}
