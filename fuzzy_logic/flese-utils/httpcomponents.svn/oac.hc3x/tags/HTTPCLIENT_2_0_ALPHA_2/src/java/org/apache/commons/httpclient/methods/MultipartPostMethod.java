/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/MultipartPostMethod.java,v 1.6 2003/01/23 22:48:08 jsdever Exp $
 * $Revision: 1.6 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
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

package org.apache.commons.httpclient.methods;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.io.File;
import java.io.OutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import org.apache.commons.httpclient.*;
import org.apache.commons.httpclient.methods.multipart.*;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * POST Method for Multipart encoded forms.
 *
 * @author <a href="mailto:mattalbright@yahoo.com">Matthew Albright</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:adrian@ephox.com">Adrian Sutton</a>
 * @author <a href="mailto:mdiggory@latte.harvard.edu">Mark Diggory</a>
 *
 * @since 2.0
 */
public class MultipartPostMethod extends GetMethod {

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(MultipartPostMethod.class);

    private List parameters = new ArrayList();

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
     * Returns <tt>"POST"</tt>.
     * @return <tt>"POST"</tt>
     */
    public String getName() {
        return "POST";
    }

    /**
     * Clear my request body.
     */
    public void recycle() {
        log.trace("enter recycle()");
        super.recycle();
        parameters.clear();
    }


    public void addParameter(String parameterName, String parameterValue) {
        log.trace("enter addParameter(String parameterName, String parameterValue)");
        Part param = new StringPart(parameterName, parameterValue);
        parameters.add(param);
    }


    public void addParameter(String parameterName, File parameterFile) 
    throws FileNotFoundException {
        log.trace("enter addParameter(String parameterName, File parameterFile)");
        Part param = new FilePart(parameterName, parameterFile);
        parameters.add(param);
    }

    public void addParameter(String parameterName, String fileName, File parameterFile) 
    throws FileNotFoundException {
        log.trace("enter addParameter(String parameterName, String fileName, File parameterFile)");
        Part param = new FilePart(parameterName, fileName, parameterFile);
        parameters.add(param);
    }
        
    /**
     * Adds another part to this post.
     */
    public void addPart( Part part ) {
        log.trace("enter addPart(Part part)");
        parameters.add(part);
    }

    protected void addRequestHeaders(HttpState state, HttpConnection conn) 
    throws IOException, HttpException {
        log.trace("enter addRequestHeaders(HttpState state, HttpConnection conn)");
        super.addRequestHeaders(state,conn);
        
        if (! parameters.isEmpty()) 
        {
            setRequestHeader("Content-Type", 
                 "multipart/form-data; boundary=" + Part.getBoundary());
        }
    }

    /**
     * Override method of {@link HttpMethodBase}
     * to write request parameters as the
     * request body.
     */
    protected boolean writeRequestBody(HttpState state, HttpConnection conn) 
    throws IOException, HttpException {
        log.trace("enter writeRequestBody(HttpState state, HttpConnection conn)");
        OutputStream out = conn.getRequestOutputStream();
        
        for (Iterator it = parameters.iterator(); it.hasNext();) {
            Part part = (Part)it.next();
            part.send(out);
        }
        
        Part.sendLastBoundary(out);

        out.flush();
        
        return true;
    }

    /**
     * Override method of {@link HttpMethodBase}
     * to return the length of the request body.
     *
     * Once this method has been invoked,
     * the request parameters cannot be altered
     * until I am {@link #recycle recycled}.
     */
    protected int getRequestContentLength() {
        log.trace("enter getRequestContentLength()");
        long length = 0;
        
        try {
            for (Iterator it = parameters.iterator(); it.hasNext();) {
                Part part = (Part)it.next();
            
                length += part.length();
            }
            length += Part.lengthOfLastBoundary();

        } catch (IOException e) {
            // Can't throw an IOException and still override
            throw new RuntimeException(e.toString());
        }
        
        // Chop the length to the max int value.
        if (length <= Integer.MAX_VALUE) {
            return((new Long(length)).intValue());
        } else {
            return(Integer.MAX_VALUE);
        }
    }
}
