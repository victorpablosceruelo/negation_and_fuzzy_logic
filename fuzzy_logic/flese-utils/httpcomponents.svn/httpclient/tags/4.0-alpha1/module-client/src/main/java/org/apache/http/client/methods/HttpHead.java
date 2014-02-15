/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/main/java/org/apache/http/client/methods/HttpHead.java $
 * $Revision: 528433 $
 * $Date: 2007-04-13 12:36:05 +0200 (Fri, 13 Apr 2007) $
 *
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.http.client.methods;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * HTTP HEAD method.
 * <p>
 * The HTTP HEAD method is defined in section 9.4 of 
 * <a href="http://www.ietf.org/rfc/rfc2616.txt">RFC2616</a>:
 * <blockquote>
 * The HEAD method is identical to GET except that the server MUST NOT
 * return a message-body in the response. The metainformation contained
 * in the HTTP headers in response to a HEAD request SHOULD be identical
 * to the information sent in response to a GET request. This method can
 * be used for obtaining metainformation about the entity implied by the
 * request without transferring the entity-body itself. This method is
 * often used for testing hypertext links for validity, accessibility,
 * and recent modification.
 * </blockquote>
 * </p>
 *
 * @version $Revision: 528433 $
 * 
 * @since 4.0
 */
public class HttpHead extends HttpRequestBase {

    public final static String METHOD_NAME = "HEAD";
    
    public HttpHead() {
        super();
    }

    public HttpHead(final URI uri) {
        super();
        setURI(uri);
    }

    public HttpHead(final String uri) throws URISyntaxException {
        super();
        setURI(new URI(uri));
    }

    public String getMethod() {
        return METHOD_NAME;
    }
    
}
