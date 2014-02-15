/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/MultipartPostMethod.java,v 1.17 2003/04/07 19:23:36 olegk Exp $
 * $Revision: 1.17 $
 * $Date: 2003-04-07 21:23:36 +0200 (Mon, 07 Apr 2003) $
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

package org.apache.commons.httpclient.methods;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.methods.multipart.FilePart;
import org.apache.commons.httpclient.methods.multipart.Part;
import org.apache.commons.httpclient.methods.multipart.StringPart;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * POST Method for Multipart encoded forms.
 *
 * @author <a href="mailto:mattalbright@yahoo.com">Matthew Albright</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:adrian@ephox.com">Adrian Sutton</a>
 * @author <a href="mailto:mdiggory@latte.harvard.edu">Mark Diggory</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 *
 * @since 2.0
 */
public class MultipartPostMethod extends ExpectContinueMethod {

    /** The Content-Type for multipart/form-data. */
    public static final String MULTIPART_FORM_CONTENT_TYPE = 
        "multipart/form-data";

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(MultipartPostMethod.class);

    /** The parameters for this method */
    private final List parameters = new ArrayList();

    /**
     * No-arg constructor.
     */
    public MultipartPostMethod() {
        super();
    }

    /**
     * Constructor specifying a URI.
     *
     * @param uri either an absolute or relative URI
     */
    public MultipartPostMethod(String uri) {
        super(uri);
    }

    /**
     * Constructor specifying a URI and tempDir.
     *
     * @param uri either an absolute or relative URI
     * @param tempDir directory to store temp files in
     */
    public MultipartPostMethod(String uri, String tempDir) {
        super(uri, tempDir);
    }

    /**
     * Constructor specifying a URI, tempDir and tempFile.
     *
     * @param uri either an absolute or relative URI
     * @param tempDir directory to store temp files in
     * @param tempFile file to store temporary data in
     */
    public MultipartPostMethod(String uri, String tempDir, String tempFile) {
        super(uri, tempDir, tempFile);
    }

    /**
     * Returns <tt>true</tt> 
     * 
     * @return <tt>true</tt>
     * 
     * @since 2.0beta1
     */
    protected boolean hasRequestContent() {
        return true;
    }

    /**
     * Returns <tt>"POST"</tt>.
     * @return <tt>"POST"</tt>
     */
    public String getName() {
        return "POST";
    }

    /**
     * Add a parameter
     * @param parameterName The name of the parameter.
     * @param parameterValue The value of the parameter.
     */
    public void addParameter(String parameterName, String parameterValue) {
        LOG.trace("enter addParameter(String parameterName, String parameterValue)");
        Part param = new StringPart(parameterName, parameterValue);
        parameters.add(param);
    }

    /**
     * Add a parameter
     * @param parameterName The name of the parameter
     * @param parameterFile The name of the file.
     * @throws FileNotFoundException If the file cannot be found.
     */
    public void addParameter(String parameterName, File parameterFile) 
    throws FileNotFoundException {
        LOG.trace("enter MultipartPostMethod.addParameter(String parameterName, "
            + "File parameterFile)");
        Part param = new FilePart(parameterName, parameterFile);
        parameters.add(param);
    }

    /**
     * Add a parameter.
     * 
     * @param parameterName The name of the parameter
     * @param fileName The file name
     * @param parameterFile The file
     * @throws FileNotFoundException If the file cannot be found.
     */
    public void addParameter(String parameterName, String fileName, File parameterFile) 
    throws FileNotFoundException {
        LOG.trace("enter MultipartPostMethod.addParameter(String parameterName, "
            + "String fileName, File parameterFile)");
        Part param = new FilePart(parameterName, fileName, parameterFile);
        parameters.add(param);
    }
        
    /**
     * Adds another part to this post.
     * @param part The part to add.
     */
    public void addPart (final Part part) {
        LOG.trace("enter addPart(Part part)");
        parameters.add(part);
    }

    /**
     * Return all parts.
     * 
     * @return an array of containing all parts
     */
    public Part[] getParts() {
        return (Part[]) parameters.toArray(new Part[parameters.size()]);
    }
    /**
     * Add content type header and set the <tt>Expect</tt> header 
     * if it has not already been set, in addition to the "standard"
     * set of headers
     * 
     * @param state the client state
     * @param conn the {@link HttpConnection} the headers will eventually be
     *        written to
     * 
     * @throws IOException when an error occurs writing the request
     * @throws HttpException when a HTTP protocol error occurs
     */
    protected void addRequestHeaders(HttpState state, HttpConnection conn) 
    throws IOException, HttpException {
        LOG.trace("enter MultipartPostMethod.addRequestHeaders(HttpState state, "
            + "HttpConnection conn)");
        super.addRequestHeaders(state, conn);
        
        if (!parameters.isEmpty()) {
            StringBuffer buffer = new StringBuffer(MULTIPART_FORM_CONTENT_TYPE);
            if (Part.getBoundary() != null) {
                buffer.append("; boundary=");
                buffer.append(Part.getBoundary());
            }
            setRequestHeader("Content-Type", buffer.toString());
        }
    }

    /**
     * Write the request body.
     * 
     * @param state the client state
     * @param conn the connection to write to
     *
     * @return <tt>true</tt>
     * @throws IOException when i/o errors occur reading the response
     * @throws HttpException when a protocol error occurs or state is invalid
     */
    protected boolean writeRequestBody(HttpState state, HttpConnection conn) 
    throws IOException, HttpException {
        LOG.trace("enter MultipartPostMethod.writeRequestBody(HttpState state, "
            + "HttpConnection conn)");
        OutputStream out = conn.getRequestOutputStream();
        Part.sendParts(out, getParts());
        return true;
    }

    /**
     * <p>Return the length of the request body.</p>
     *
     * <p>Once this method has been invoked, the request parameters cannot be
     * altered until I am {@link #recycle recycled}.</p>
     * 
     * @return The request content length.
     */
    protected int getRequestContentLength() {
        LOG.trace("enter MultipartPostMethod.getRequestContentLength()");
        try {
            long len = Part.getLengthOfParts(getParts());
            // Chop the length to the max int value.
            if (len <= Integer.MAX_VALUE) {
                return (int) len;
            } else {
                return (Integer.MAX_VALUE);
            }
        } catch (IOException e) {
            // Can't throw an IOException and still override
            throw new RuntimeException(e.toString());
        }
    }


    /**
     * Clear my request body.
     */
    public void recycle() {
        LOG.trace("enter MultipartPostMethod.recycle()");
        super.recycle();
        parameters.clear();
    }
}
