/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/HttpUrlMethod.java,v 1.9.2.1 2004/02/22 18:21:13 olegk Exp $
 * $Revision: 1.9.2.1 $
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

package org.apache.commons.httpclient;

import java.net.MalformedURLException;

/**
 * HttpUrlMethod extends HttpMethod.  HttpMethod only contains the path portion
 * of a URL and gets the host:port from the connection maintained in
 * HttpCleint.  HttpUrlMethod is initialized with a fully specified URL and is
 * used with HttpMultiClient.  HttpMultiClient  chooses the appropriate
 * HttpConnectoin (via MultiThreadedHttpConnectionManager) based on the host and port in
 * the URL.
 * 
 * @deprecated use HttpMethod
 * 
 * @author Marc A. Saegesser
 */
public interface HttpUrlMethod extends HttpMethod {
    //~ Methods ································································

    /**
     * Sets the URL.  Calls the underlying HttpMethod.setPath() with the url's
     * path.  If the url contains a query string the underlying
     * HttpMethod.setQueryString() is called.
     * 
     * @param url - the URL for this request.
     * @throws MalformedURLException when the <i>url</i> can't be created
     */
    void setUrl(String url) throws MalformedURLException;

    /**
     * Returns this request's URL.
     * 
     * @return the request's URL.
     */
    String getUrl();
}
