/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/PutMethod.java,v 1.1 2001/04/25 18:42:52 remm Exp $
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
            if (nb == -1)
                break;
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
            if (nb == -1)
                break;
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


}
