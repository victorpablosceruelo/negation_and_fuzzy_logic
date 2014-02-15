/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/PostMethod.java,v 1.4 2001/09/01 21:40:46 remm Exp $
 * $Revision: 1.4 $
 * $Date: 2001-09-01 23:40:46 +0200 (Sat, 01 Sep 2001) $
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


/**
 * POST Method.
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author <a href="mailto:dsale@us.britannica.com">Doug Sale</a>
 */
public class PostMethod
    extends GetMethod {
    
    
    // -------------------------------------------------------------- Constants
    
    
    protected static final String POST = "POST";
    
    
    // ----------------------------------------------------------- Constructors
    
    
    /**
     * Method constructor.
     */
    public PostMethod() {
        super();
        name = POST;
    }
    
    
    /**
     * Method constructor.
     */
    public PostMethod(String path) {
        super(path);
        name = POST;
    }
    
    
    /**
     * Method constructor.
     */
    public PostMethod(String path, String tempDir) {
        super(path, tempDir);
        name = POST;
    }
    
    
    
    
    /**
     * Method constructor.
     */
    public PostMethod(String path, boolean useDisk, String tempDir) {
        super(path, useDisk, tempDir);
        name = POST;
    }
    
    
    /**
     * Method constructor.
     */
    public PostMethod(String path, boolean useDisk, String tempDir,
                      String tempFile) {
        super(path, useDisk, tempDir, tempFile);
        name = POST;
    }
    
    
    // ----------------------------------------------------- Instance Variables
    
    
    /**
     * Parameters hashtable.
     */
    Hashtable parameters = new Hashtable();
    
    
    // --------------------------------------------------------- Public Methods
    
    
    /**
     * Add parameter.
     */
    public void addParameter(String name, String value) {
        checkNotUsed();
        parameters.put(name, value);
    }
    
    
    // ----------------------------------------------------- HttpMethod Methods
    
    
    public void recycle() {
        super.recycle();
        parameters.clear();
    }
    
    
    /**
     * Generate additional headers needed by the request.
     *
     * @param host the host
     * @param state State token
     */
    public void generateHeaders(String host, State state) {
        
        super.generateHeaders(host, state);
        
        if (!parameters.isEmpty()) {
            super.setHeader("Content-Type", 
                            "application/x-www-form-urlencoded");
        }
        
    }
    
    
    /**
     * Generate the query body.
     *
     * @return String query
     */
    public String generateQuery() {
        if (!parameters.isEmpty()) {
            StringBuffer sb = new StringBuffer();
            Enumeration names = parameters.keys();
            while (names.hasMoreElements()) {
                String name = (String) names.nextElement();
                String value = (String) parameters.get(name);
                sb.append(URLEncoder.encode(name));
                sb.append("=");
                sb.append(URLEncoder.encode(value));
                if (names.hasMoreElements())
                    sb.append("&");
            }
            return sb.toString();
        } else {
            return null;
        }
    }
    
    
    /**
     * from RFC1945:<BR>
     *   A valid Content-Length is required on all HTTP/1.0 POST requests. An
     *   HTTP/1.0 server should respond with a 400 (bad request) message if it
     *   cannot determine the length of the request message's content.
     * 
     * @return true
     */
    public boolean needContentLength() {
      return true;
    }

    /**
     * Parse response.
     *
     * @param is Input stream
     */
    /*
    public void parseResponse(InputStream is)
        throws IOException {
    }
    */
}
