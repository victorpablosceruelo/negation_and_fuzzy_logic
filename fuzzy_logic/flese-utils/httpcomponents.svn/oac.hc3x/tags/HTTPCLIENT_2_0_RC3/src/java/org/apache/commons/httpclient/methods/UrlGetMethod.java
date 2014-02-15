/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/Attic/UrlGetMethod.java,v 1.12 2003/01/30 05:01:56 jsdever Exp $
 * $Revision: 1.12 $
 * $Date: 2004-01-17 06:43:14 +0100 (Sat, 17 Jan 2004) $
 *
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

import org.apache.commons.httpclient.HttpUrlMethod;
import org.apache.commons.httpclient.util.URIUtil;

import java.io.File;
import java.net.MalformedURLException;

/**
 * Implements the URL version of GetMethod.  It serves the
 * same purpose as GetMethod but it takes  URL instead of
 * a path.  
 *
 * @deprecated use GetMethod
 *
 * @author Marc A. Saegesser
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 */
public class UrlGetMethod extends GetMethod implements HttpUrlMethod {
    // ----------------------------------------------------- Instance Variables
    /** The URL */
    private String url;

    /**
     * No-arg constructor.
     */
    public UrlGetMethod() {
        super();
    }

    /**
     * Create an instance with the specified URL
     * @param url The URL
     * @throws MalformedURLException If the url isn't valid.
     */
    public UrlGetMethod(String url) throws MalformedURLException {
        super(URIUtil.getPath(url));
        setUrl(url);
    }

    /**
     * Create an instance with the specified URL and temporary directory.
     * @param url The URL
     * @param tempDir The temporary directory
     * @throws MalformedURLException If the url isn't valid.
     */
    public UrlGetMethod(String url, String tempDir) throws MalformedURLException {
        super(URIUtil.getPath(url), tempDir);
        setUrl(url);
    }

    /**
     * Constructor.
     * @param url the path of the request
     * @param tempDir the directory in which to store temporary files
     * @param tempFile the file (under tempDir) to buffer contents to
     * @throws MalformedURLException If the url isn't valid.
     */
    public UrlGetMethod(String url, String tempDir, String tempFile) 
        throws MalformedURLException {
        super(URIUtil.getPath(url), tempDir, tempFile);
        setUrl(url);
    }

    /**
     * Constructor.
     * @param url the path of the request
     * @param fileData the file to buffer contents to
     * @throws MalformedURLException If the url isn't valid.
     */
    public UrlGetMethod(String url, File fileData) 
        throws MalformedURLException {
        super(URIUtil.getPath(url), fileData);
        setUrl(url);
    }

    /**
     * Sets the URL.  Calls the underlying HttpMethod.setPath()
     * with the url's path.  If the url contains a query string
     * the underlying HttpMethod.setQueryString() is called.
     *
     * @param url - the URL for this request.
     * @throws MalformedURLException If the url isn't valid.
     */
    public void setUrl(String url) throws MalformedURLException {
        super.setPath(URIUtil.getPath(url));
        this.url = url;
        String query = URIUtil.getQuery(url);
        if (query != null && query.length() > 0) {
            super.setQueryString(query);
        }
    }

    /**
     * Returns this request's URL.
     * 
     * @return the request's URL.
     */
    public String getUrl() {
        return url;
    }
}
