/*
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
package org.apache.http.impl.client.cache;

import java.util.Date;

import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.ProtocolVersion;
import org.apache.http.annotation.Immutable;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.impl.client.RequestWrapper;
import org.apache.http.impl.cookie.DateUtils;

/**
 * @since 4.1
 */
@Immutable
public class ResponseProtocolCompliance {

    /**
     *
     * @param request
     * @param response
     * @throws ClientProtocolException
     */
    public void ensureProtocolCompliance(HttpRequest request, HttpResponse response)
            throws ClientProtocolException {
        if (backendResponseMustNotHaveBody(request, response)) {
            response.setEntity(null);
        }

        authenticationRequiredDidNotHaveAProxyAuthenticationHeader(request, response);

        notAllowedResponseDidNotHaveAnAllowHeader(request, response);

        unauthorizedResponseDidNotHaveAWWWAuthenticateHeader(request, response);

        requestDidNotExpect100ContinueButResponseIsOne(request, response);

        transferEncodingIsNotReturnedTo1_0Client(request, response);

        ensurePartialContentIsNotSentToAClientThatDidNotRequestIt(request, response);

        ensure200ForOPTIONSRequestWithNoBodyHasContentLengthZero(request, response);

        ensure206ContainsDateHeader(response);
    }

    private void authenticationRequiredDidNotHaveAProxyAuthenticationHeader(HttpRequest request,
            HttpResponse response) throws ClientProtocolException {
        if (response.getStatusLine().getStatusCode() != HttpStatus.SC_PROXY_AUTHENTICATION_REQUIRED)
            return;

        if (response.getFirstHeader("Proxy-Authenticate") == null)
            throw new ClientProtocolException(
                    "407 Response did not contain a Proxy-Authentication header");
    }

    private void notAllowedResponseDidNotHaveAnAllowHeader(HttpRequest request,
            HttpResponse response) throws ClientProtocolException {
        if (response.getStatusLine().getStatusCode() != HttpStatus.SC_METHOD_NOT_ALLOWED)
            return;

        if (response.getFirstHeader("Allow") == null)
            throw new ClientProtocolException("405 Response did not contain an Allow header.");
    }

    private void unauthorizedResponseDidNotHaveAWWWAuthenticateHeader(HttpRequest request,
            HttpResponse response) throws ClientProtocolException {
        if (response.getStatusLine().getStatusCode() != HttpStatus.SC_UNAUTHORIZED)
            return;

        if (response.getFirstHeader("WWW-Authenticate") == null) {
            throw new ClientProtocolException(
                    "401 Response did not contain required WWW-Authenticate challenge header");
        }
    }

    private void ensure206ContainsDateHeader(HttpResponse response) {
        if (response.getFirstHeader(HeaderConstants.DATE) == null) {
            response.addHeader(HeaderConstants.DATE, DateUtils.formatDate(new Date()));
        }

    }

    private void ensurePartialContentIsNotSentToAClientThatDidNotRequestIt(HttpRequest request,
            HttpResponse response) throws ClientProtocolException {
        if (request.getFirstHeader(HeaderConstants.RANGE) != null)
            return;

        if (response.getFirstHeader(HeaderConstants.CONTENT_RANGE) != null) {
            throw new ClientProtocolException(
                    "Content-Range was returned for a request that did not ask for a Content-Range.");
        }

    }

    private void ensure200ForOPTIONSRequestWithNoBodyHasContentLengthZero(HttpRequest request,
            HttpResponse response) {
        if (!request.getRequestLine().getMethod().equalsIgnoreCase(HeaderConstants.OPTIONS_METHOD)) {
            return;
        }

        if (response.getStatusLine().getStatusCode() != HttpStatus.SC_OK) {
            return;
        }

        if (response.getFirstHeader(HeaderConstants.CONTENT_LENGTH) == null) {
            response.addHeader(HeaderConstants.CONTENT_LENGTH, "0");
        }
    }

    private boolean backendResponseMustNotHaveBody(HttpRequest request, HttpResponse backendResponse) {
        return HeaderConstants.HEAD_METHOD.equals(request.getRequestLine().getMethod())
                || backendResponse.getStatusLine().getStatusCode() == HttpStatus.SC_NO_CONTENT
                || backendResponse.getStatusLine().getStatusCode() == HttpStatus.SC_RESET_CONTENT
                || backendResponse.getStatusLine().getStatusCode() == HttpStatus.SC_NOT_MODIFIED;
    }

    private void requestDidNotExpect100ContinueButResponseIsOne(HttpRequest request,
            HttpResponse response) throws ClientProtocolException {
        if (response.getStatusLine().getStatusCode() != HttpStatus.SC_CONTINUE) {
            return;
        }

        if (!requestWasWrapped(request)) {
            return;
        }

        ProtocolVersion originalProtocol = getOriginalRequestProtocol((RequestWrapper) request);

        if (originalProtocol.compareToVersion(CachingHttpClient.HTTP_1_1) >= 0) {
            return;
        }

        if (originalRequestDidNotExpectContinue((RequestWrapper) request)) {
            throw new ClientProtocolException("The incoming request did not contain a "
                    + "100-continue header, but the response was a Status 100, continue.");

        }
    }

    private void transferEncodingIsNotReturnedTo1_0Client(HttpRequest request, HttpResponse response) {
        if (!requestWasWrapped(request)) {
            return;
        }

        ProtocolVersion originalProtocol = getOriginalRequestProtocol((RequestWrapper) request);

        if (originalProtocol.compareToVersion(CachingHttpClient.HTTP_1_1) >= 0) {
            return;
        }

        removeResponseTransferEncoding(response);
    }

    private void removeResponseTransferEncoding(HttpResponse response) {
        response.removeHeaders("TE");
        response.removeHeaders(HeaderConstants.TRANSFER_ENCODING);
    }

    private boolean originalRequestDidNotExpectContinue(RequestWrapper request) {

        try {
            HttpEntityEnclosingRequest original = (HttpEntityEnclosingRequest) request
                    .getOriginal();

            return !original.expectContinue();
        } catch (ClassCastException ex) {
            return false;
        }
    }

    private ProtocolVersion getOriginalRequestProtocol(RequestWrapper request) {
        return request.getOriginal().getProtocolVersion();
    }

    private boolean requestWasWrapped(HttpRequest request) {
        return request instanceof RequestWrapper;
    }

}
