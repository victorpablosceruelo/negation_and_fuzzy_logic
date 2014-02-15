/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/OptionsMethod.java,v 1.1 2001/04/25 18:42:52 remm Exp $
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

import java.io.*;
import java.util.*;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.State;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpMethodBase;


/**
 * OPTIONS Method.
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 */
public class OptionsMethod
    extends HttpMethodBase {
    
    
    // -------------------------------------------------------------- Constants
    
    
    // ----------------------------------------------------------- Constructors
    
    
    /**
     * Method constructor.
     */
    public OptionsMethod() {
        name = "OPTIONS";
    }
    
    
    /**
     * Method constructor.
     */
    public OptionsMethod(String path) {
        super(path);
        name = "OPTIONS";
    }
    
    
    // ----------------------------------------------------- Instance Variables
    
    
    /**
     * Methods allowed.
     */
    private Vector methodsAllowed = new Vector();
    
    
    // --------------------------------------------------------- Public Methods
    
    
    /**
     * Is the specified method allowed ?
     */
    public boolean isAllowed(String method) {
        checkUsed();
        return methodsAllowed.contains(method);
    }
    
    
    /**
     * Get a list of allowed methods.
     */
    public Enumeration getAllowedMethods() {
        checkUsed();
        return methodsAllowed.elements();
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
    }
    
    
    /**
     * Process response headers. The contract of this method is that it only
     * parses the response headers.
     *
     * @param headers Headers list
     */
    public void processResponseHeaders(Hashtable headers) {
        
        Header allowHeader = (Header) headers.get("allow");
        if (allowHeader != null) {
            String allowHeaderValue = allowHeader.getValue();
            StringTokenizer tokenizer =
                new StringTokenizer(allowHeaderValue, ",");
            while (tokenizer.hasMoreElements()) {
                String methodAllowed =
                    tokenizer.nextToken().trim().toUpperCase();
                methodsAllowed.addElement(methodAllowed);
            }
        }
    }
    
    
}
