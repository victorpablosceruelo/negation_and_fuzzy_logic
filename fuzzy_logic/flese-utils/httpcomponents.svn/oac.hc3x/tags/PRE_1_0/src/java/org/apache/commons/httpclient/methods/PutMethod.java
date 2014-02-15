/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/PutMethod.java,v 1.4 2001/08/10 17:17:39 rwaldhoff Exp $
 * $Revision: 1.4 $
 * $Date: 2001-08-10 19:17:39 +0200 (Fri, 10 Aug 2001) $
 *
 * Copyright (C) The Apache Software Foundation. All rights reserved.
 *
 * This software is published under the terms of the Apache Software License
 * version 1.1, a copy of which has been included with this distribution in
 * the LICENSE file.
 *
 */

package org.apache.commons.httpclient.methods;

import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.net.URL;
import java.net.URLConnection;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.State;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpMethodBase;
import org.apache.commons.httpclient.log.Log;
import org.apache.commons.httpclient.log.LogSource;


/**
 * PUT Method.
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 */
public class PutMethod
    extends HttpMethodBase {


    // ----------------------------------------------------------- Constructors


    /**
     * Method constructor.
     */
    public PutMethod() {
        name = "PUT";
    }


    /**
     * Method constructor.
     */
    public PutMethod(String path) {
        super(path);
        name = "PUT";
    }


    // ------------------------------------------------------- Instance Methods


    /**
     * Send byte buffer.
     */
    private byte[] data = null;


    /**
     * Send file contents.
     */
    private File file = null;


    /**
     * Set content from URL.
     */
    private URL url = null;


    // --------------------------------------------------------- Public Methods


    public void setDebug(int debug) {
        // log.setLevel(debug);
        // if I had a basic log
    }

    /**
     * Send the contents of a file.
     */
    public void sendData(File file)
        throws IOException {
        checkNotUsed();
        this.file = file;
    }


    /**
     * Send the contents of the resource at the specified URL.
     */
    public void sendData(URL url)
        throws IOException {
        checkNotUsed();
        this.url = url;
    }


    /**
     * Send the contents of a byte array.
     */
    public void sendData(byte[] data) {
        checkNotUsed();
        this.data = data;
    }


    /**
     * Send the contents of a string.
     */
    public void sendData(String data) {
        checkNotUsed();
        sendData(data.getBytes());
    }


    /**
     * Send the contents of an input stream. The contents will be buffered into
     * memory. To upload large entities, it is recommended to first buffer the
     * data into a temporary file, and then send that file.
     */
    public void sendData(InputStream is)
        throws IOException {
        checkNotUsed();
        byte[] buffer = new byte[4096];
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        int nb = 0;
        while (true) {
            nb = is.read(buffer);
            if (nb == -1) {
                break;
            }
            os.write(buffer, 0, nb);
        }
        data = os.toByteArray();
    }


    // ----------------------------------------------------- HttpMethod Methods


    /**
     * Is the query body submitted through an InputStream of with a String.
     * If an InputStream is available, it's used.
     *
     * @return boolean True if the content is avalable in an InputStream
     */
    public boolean isStreamedQuery() {
        return ((file != null) || (url != null));
    }


    /**
     * Recycle the method object, so that it can be reused again. Any attempt
     * to reuse an object without recycling it will throw a WebdavException.
     */
    public void recycle() {
        super.recycle();
        data = null;
        url = null;
        file = null;
    }


    /**
     * Generate the query body.
     *
     * @return String query
     */
    public String generateQuery() {
        if (query != null){
            return query;
        }
        if (data == null) {
            return "";
        } else {
            return new String(data);
        }
    }


    /**
     * Stream the body of the query. This function should be used to send large
     * request bodies.
     */
    public void streamQuery(OutputStream out)
        throws IOException {

        InputStream inputStream = null;
        if (file != null) {
            inputStream = new FileInputStream(file);
        } else if (url != null) {
            inputStream = url.openConnection().getInputStream();
        }

        byte[] buffer = new byte[4096];
        int nb = 0;
        while (true) {
            nb = inputStream.read(buffer);
            if (nb == -1) {
                break;
            }
            if(wireLog.isInfoEnabled()) {
                wireLog.info(">> \"" + new String(buffer) + "\"");
            }
            out.write(buffer, 0, nb);
        }

        inputStream.close();

    }


    /**
     * Parse response.
     *
     * @param is Input stream
     */
    public void parseResponse(InputStream is)
        throws IOException {
    }


    /**
     * Return true if the method should ask for an expectation.
     *
     * @return true if an expectation will be sent
     */
    public boolean needExpectation() {
        return true;
    }


    static protected final Log wireLog = LogSource.getInstance("httpclient.wire");
}
