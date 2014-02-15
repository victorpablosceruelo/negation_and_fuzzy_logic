/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/impl/NotificationResponseWrapper.java $
 * $Revision: 505746 $
 * $Date: 2007-02-10 19:59:14 +0100 (Sat, 10 Feb 2007) $
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

package org.apache.http.async.impl;


import java.util.Locale;
import java.util.Iterator;

import org.apache.http.HttpResponse;
import org.apache.http.HttpVersion;
import org.apache.http.StatusLine;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.params.HttpParams;



/**
 * Response object wrapper for notification.
 * A notification handler is not allowed to access the response entity,
 * and this wrapper will not allow it to.
 *
 *
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 505746 $ $Date: 2007-02-10 19:59:14 +0100 (Sat, 10 Feb 2007) $
 * 
 * @since 4.0
 */
public final class NotificationResponseWrapper
    implements HttpResponse {


    /** The response being wrapped. */
    private final HttpResponse wrapped_response;


    /**
     * Creates a new response wrapper for notification.
     *
     * @param response  the response to wrap
     */
    public NotificationResponseWrapper(HttpResponse response) {

        if (response == null)
            throw new IllegalArgumentException("response must not be null");

        wrapped_response = response;

    } // constructor



    /**
     * Disabled operation.
     * This method is disabled since notification handlers
     * are not allowed to call it.
     *
     * @return  never
     *
     * @throws IllegalStateException    always
     */
    public HttpEntity getEntity()
        throws IllegalStateException {

        throw new IllegalStateException("notification thread abuse");
    }


    /**
     * Disabled operation.
     * This method is disabled since notification handlers
     * are not allowed to call it.
     *
     * @throws IllegalStateException    always
     */
    public void setEntity(HttpEntity entity)
        throws IllegalStateException {

        throw new IllegalStateException("notification thread abuse");
    }


    // non-javadoc, see interface HttpResponse
    public StatusLine getStatusLine() {
        return wrapped_response.getStatusLine();
    }

    // non-javadoc, see interface HttpResponse
    public void setStatusLine(StatusLine statusline) {
        wrapped_response.setStatusLine(statusline);
    }

    // non-javadoc, see interface HttpResponse
    public void setStatusLine(HttpVersion ver, int code) {
        wrapped_response.setStatusLine(ver, code);
    }

    // non-javadoc, see interface HttpResponse
    public void setStatusLine(HttpVersion ver, int code, String reason) {
        wrapped_response.setStatusLine(ver, code, reason);
    }

    // non-javadoc, see interface HttpResponse
    public void setStatusCode(int code) {
        wrapped_response.setStatusCode(code);
    }

    // non-javadoc, see interface HttpResponse
    public void setReasonPhrase(String reason) {
        wrapped_response.setReasonPhrase(reason);
    }

    // non-javadoc, see interface HttpResponse
    public Locale getLocale() {
        return wrapped_response.getLocale();
    }

    // non-javadoc, see interface HttpResponse
    public void setLocale(Locale loc) {
        wrapped_response.setLocale(loc);
    }




    // non-javadoc, see interface HttpMessage
    public HttpVersion getHttpVersion() {
        return wrapped_response.getHttpVersion();
    }

    // non-javadoc, see interface HttpMessage
    public boolean containsHeader(String name) {
        return wrapped_response.containsHeader(name);
    }

    // non-javadoc, see interface HttpMessage
    public Header[] getHeaders(String name) {
        return wrapped_response.getHeaders(name);
    }

    // non-javadoc, see interface HttpMessage
    public Header getFirstHeader(String name) {
        return wrapped_response.getFirstHeader(name);
    }

    // non-javadoc, see interface HttpMessage
    public Header getLastHeader(String name) {
        return wrapped_response.getLastHeader(name);
    }

    // non-javadoc, see interface HttpMessage
    public Header[] getAllHeaders() {
        return wrapped_response.getAllHeaders();
    }

    // non-javadoc, see interface HttpMessage
    public void addHeader(Header header) {
        wrapped_response.addHeader(header);
    }

    // non-javadoc, see interface HttpMessage
    public void addHeader(String name, String value) {
        wrapped_response.addHeader(name, value);
    }

    // non-javadoc, see interface HttpMessage
    public void setHeader(Header header) {
        wrapped_response.setHeader(header);
    }

    // non-javadoc, see interface HttpMessage
    public void setHeader(String name, String value) {
        wrapped_response.setHeader(name, value);
    }

    // non-javadoc, see interface HttpMessage
    public void setHeaders(Header[] headers) {
        wrapped_response.setHeaders(headers);
    }

    // non-javadoc, see interface HttpMessage
    public void removeHeader(Header header) {
        wrapped_response.removeHeader(header);
    }

    // non-javadoc, see interface HttpMessage
    public void removeHeaders(String name) {
        wrapped_response.removeHeaders(name);
    }

    // non-javadoc, see interface HttpMessage
    public Iterator headerIterator() {
        return wrapped_response.headerIterator();
    }

    // non-javadoc, see interface HttpMessage
    public HttpParams getParams() {
        return wrapped_response.getParams();
    }

    // non-javadoc, see interface HttpMessage
    public void setParams(HttpParams params) {
        wrapped_response.setParams(params);
    }


} // class NotificationResponseWrapper

