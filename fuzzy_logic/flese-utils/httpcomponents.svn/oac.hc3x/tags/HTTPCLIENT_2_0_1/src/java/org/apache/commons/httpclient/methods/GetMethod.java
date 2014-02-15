/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/GetMethod.java,v 1.24.2.3 2004/06/13 20:24:49 olegk Exp $
 * $Revision: 1.24.2.3 $
 * $Date: 2004-06-13 22:24:49 +0200 (Sun, 13 Jun 2004) $
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
 * Implements the HTTP GET method.
 * <p>
 * The HTTP GET method is defined in section 9.3 of
 * <a href="http://www.ietf.org/rfc/rfc2616.txt">RFC2616</a>:
 * <blockquote>
 * The GET method means retrieve whatever information (in the form of an
 * entity) is identified by the Request-URI. If the Request-URI refers
 * to a data-producing process, it is the produced data which shall be
 * returned as the entity in the response and not the source text of the
 * process, unless that text happens to be the output of the process.
 * </blockquote>
 * </p>
 * <p>
 * GetMethods will follow redirect requests from the http server by default.
 * This behavour can be disabled by calling setFollowRedirects(false).</p>
 * <p>
 * The useDisk methods have been deprecated.  Disk I/O is the responsibility
 * of the client.  If you need to write a response body to a file, you
 * can use the following as an example:
 * <pre>
 *     out = new FileOutputStream(myFile);
 *     InputStream in = getResponseBodyAsStream();
 *     byte[] buffer = new byte[10000];
 *     int len ;
 *     while ((len = in.read(buffer)) > 0) {
 *        out.write(buffer, 0, len);
 *     }
 *     in.close();
 *     out.close();
 * </pre>
 * </p>
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author Sung-Gu Park
 * @author Sean C. Sullivan
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * 
 * @version $Revision: 1.24.2.3 $
 * @since 1.0
 */
public class GetMethod extends HttpMethodBase {

    // -------------------------------------------------------------- Constants

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(GetMethod.class);

    /** 
     * Temporary directory.
     * @deprecated the client is responsible for disk I/O
     */
    private static final String TEMP_DIR = "temp/";


    // ----------------------------------------------------- Instance Variables

    /** 
     * File which contains the buffered data.
     * @deprecated the client is responsible for disk I/O
     */
    private File fileData;

    /** 
     * Temporary directory to use.
     * @deprecated the client is responsible for disk I/O
     */
    private String tempDir = TEMP_DIR;

    /** 
     * Temporary file to use.
     * @deprecated the client is responsible for disk I/O
     */
    private String tempFile = null;

    /** 
     * By default, the get method will buffer read data to the memory.
     * @deprecated the client is responsible for disk I/O
     */
    private boolean useDisk = false;


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
        LOG.trace("enter GetMethod(String)");
        setFollowRedirects(true);
    }

    /**
     * Constructor.
     * 
     * @param path the path to request
     * @param tempDir the directory in which to store temporary files
     * 
     * @deprecated the client is responsible for disk I/O
     * @since 1.0
     */
    public GetMethod(String path, String tempDir) {
        super(path);
        LOG.trace("enter GetMethod(String, String)");
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
     * @deprecated the client is responsible for disk I/O
     * @since 1.0
     */
    public GetMethod(String path, String tempDir, String tempFile) {
        super(path);
        LOG.trace("enter GetMethod(String, String, String)");
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
     * @deprecated the client is responsible for disk I/O
     * @since 1.0
     */
    public GetMethod(String path, File fileData) {
        this(path);
        LOG.trace("enter GetMethod(String, File)");
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
     * @deprecated the client is responsible for disk I/O
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
     * @deprecated the client is responsible for disk I/O
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
     * Temporary directory setter.
     * 
     * @param tempDir New value of tempDir
     * 
     * @deprecated the client is responsible for disk I/O
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
     * @deprecated the client is responsible for disk I/O
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
     * @deprecated the client is responsible for disk I/O
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
     * @deprecated the client is responsible for disk I/O
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
     * @deprecated the client is responsible for disk I/O
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
     * @deprecated the client is responsible for disk I/O
     * @since 1.0
     */
    public boolean getUseDisk() {
        return useDisk;
    }

    /**
     * Recycles the HTTP method so that it can be used again.
     * Note that all of the instance variables will be reset
     * once this method has been called. This method will also
     * release the connection being used by this HTTP method.
     * 
     * @see #releaseConnection()
     * 
     * @since 1.0
     * 
     * @deprecated no longer supported and will be removed in the future
     *             version of HttpClient
     */
    public void recycle() {
        LOG.trace("enter GetMethod.recycle()");

        super.recycle();
        this.fileData = null;
        setFollowRedirects(true);
    }

    // ----------------------------------------------------- HttpMethod Methods

    /**
     * Overrides method in {@link HttpMethodBase} to write data to the
     * appropriate buffer.
     * 
     * @param state the {@link HttpState state} information associated with this method
     * @param conn the {@link HttpConnection connection} used to execute
     *        this HTTP method
     *
     * @throws IOException if an I/O (transport) error occurs
     * @throws HttpException  if a protocol exception occurs.
     * @throws HttpRecoverableException if a recoverable transport error occurs. 
     *                    Usually this kind of exceptions can be recovered from by
     *                    retrying the HTTP method 
     *
     * @see #readResponse
     * @see #processResponseBody
     * 
     * @since 2.0
     */
    protected void readResponseBody(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        LOG.trace("enter GetMethod.readResponseBody(HttpState, HttpConnection)");

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
     *
     * @deprecated the client is responsible for disk I/O
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
