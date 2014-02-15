/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/Attic/UrlHeadMethod.java,v 1.12.2.1 2004/02/22 18:21:15 olegk Exp $
 * $Revision: 1.12.2.1 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
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

import org.apache.commons.httpclient.HttpUrlMethod;
import org.apache.commons.httpclient.util.URIUtil;

import java.net.MalformedURLException;

/**
 * HttpUrlMethod version of HeadMethod.
 *
 * @deprecated use HeadMethod
 *
 * @author Marc A. Saegesser
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 */
public class UrlHeadMethod extends HeadMethod implements HttpUrlMethod {
    // ----------------------------------------------------- Instance Variables
    /** The URL */
    private String url;

    /**
     * No-arg constructor.
     */
    public UrlHeadMethod() {
        super();
    }


    /**
     * Path-setting constructor.
     * @param url the path of the request
     * @throws MalformedURLException If the url isn't valid.
     */
    public UrlHeadMethod(String url) throws MalformedURLException {
        super(URIUtil.getPath(url));
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

