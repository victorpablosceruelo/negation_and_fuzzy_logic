/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/main/java/org/apache/http/client/methods/HttpRequestBase.java $
 * $Revision: 537141 $
 * $Date: 2007-05-11 10:59:55 +0200 (Fri, 11 May 2007) $
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

import java.io.IOException;
import java.net.URI;

import org.apache.http.HttpVersion;
import org.apache.http.RequestLine;
import org.apache.http.conn.ConnectionReleaseTrigger;
import org.apache.http.message.AbstractHttpMessage;
import org.apache.http.message.BasicRequestLine;
import org.apache.http.params.HttpProtocolParams;

/**
 * Basic implementation of an HTTP request that can be modified.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 537141 $
 * 
 * @since 4.0
 */
abstract class HttpRequestBase extends AbstractHttpMessage 
    implements HttpUriRequest, AbortableHttpRequest {
    
    private URI uri;
    private ConnectionReleaseTrigger releaseTrigger;
    
    public HttpRequestBase() {
        super();
    }

    public abstract String getMethod();

    public HttpVersion getHttpVersion() {
        return HttpProtocolParams.getVersion(getParams());
    }

    public URI getURI() {
        return this.uri;
    }
    
    public RequestLine getRequestLine() {
        String method = getMethod();
        HttpVersion ver = getHttpVersion();
        URI uri = getURI();
        String uritext;
        if (uri != null) {
            uritext = uri.toASCIIString();
        } else {
            uritext = "/";
        }
        return new BasicRequestLine(method, uritext, ver);
    }

    public void setURI(final URI uri) {
        this.uri = uri;
    }

    public void setReleaseTrigger(final ConnectionReleaseTrigger releaseTrigger) {
        this.releaseTrigger = releaseTrigger;
    }
    
    public void abort() {
        if (this.releaseTrigger != null) {
            try {
                this.releaseTrigger.abortConnection();
            } catch (IOException ex) {
                // ignore
            }
        }
    }

}
