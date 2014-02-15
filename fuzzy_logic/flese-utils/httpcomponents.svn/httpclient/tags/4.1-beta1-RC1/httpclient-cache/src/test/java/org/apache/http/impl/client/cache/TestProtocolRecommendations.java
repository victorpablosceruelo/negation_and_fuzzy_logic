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

import static org.easymock.classextension.EasyMock.*;
import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Date;

import org.apache.http.Header;
import org.apache.http.HeaderElement;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.impl.cookie.DateUtils;
import org.apache.http.message.BasicHttpEntityEnclosingRequest;
import org.apache.http.message.BasicHttpRequest;
import org.apache.http.message.BasicHttpResponse;
import org.apache.http.protocol.HttpContext;
import org.easymock.Capture;
import org.easymock.EasyMock;
import org.junit.Test;

/*
 * This test class captures functionality required to achieve unconditional
 * compliance with the HTTP/1.1 spec, i.e. all the SHOULD, SHOULD NOT,
 * RECOMMENDED, and NOT RECOMMENDED behaviors.
 */
public class TestProtocolRecommendations extends AbstractProtocolTest {

    /* "identity: The default (identity) encoding; the use of no
     * transformation whatsoever. This content-coding is used only in the
     * Accept-Encoding header, and SHOULD NOT be used in the
     * Content-Encoding header."
     *
     * http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.5
     */
    @Test
    public void testIdentityCodingIsNotUsedInContentEncodingHeader()
        throws Exception {
        originResponse.setHeader("Content-Encoding", "identity");
        backendExpectsAnyRequest().andReturn(originResponse);
        replayMocks();
        HttpResponse result = impl.execute(host, request);
        verifyMocks();
        boolean foundIdentity = false;
        for(Header h : result.getHeaders("Content-Encoding")) {
            for(HeaderElement elt : h.getElements()) {
                if ("identity".equalsIgnoreCase(elt.getName())) {
                    foundIdentity = true;
                }
            }
        }
        assertFalse(foundIdentity);
    }

    /*
     * "For this reason, a cache SHOULD NOT return a stale response if the
     * client explicitly requests a first-hand or fresh one, unless it is
     * impossible to comply for technical or policy reasons."
     */
    private HttpRequest requestToPopulateStaleCacheEntry() throws Exception {
        HttpRequest req1 = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpResponse resp1 = HttpTestUtils.make200Response();
        Date now = new Date();
        Date tenSecondsAgo = new Date(now.getTime() - 10 * 1000L);
        resp1.setHeader("Date", DateUtils.formatDate(tenSecondsAgo));
        resp1.setHeader("Cache-Control","public,max-age=5");
        resp1.setHeader("Etag","\"etag\"");

        backendExpectsAnyRequest().andReturn(resp1);
        return req1;
    }

    private void testDoesNotReturnStaleResponseOnError(HttpRequest req2)
            throws Exception, IOException {
        HttpRequest req1 = requestToPopulateStaleCacheEntry();

        backendExpectsAnyRequest().andThrow(new IOException());

        replayMocks();
        impl.execute(host, req1);
        HttpResponse result = null;
        try {
            result = impl.execute(host, req2);
        } catch (IOException acceptable) {
        }
        verifyMocks();

        if (result != null) {
            assertFalse(result.getStatusLine().getStatusCode() == HttpStatus.SC_OK);
        }
    }

    @Test
    public void testDoesNotReturnStaleResponseIfClientExplicitlyRequestsFirstHandOneWithCacheControl()
            throws Exception {
        HttpRequest req = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        req.setHeader("Cache-Control","no-cache");
        testDoesNotReturnStaleResponseOnError(req);
    }

    @Test
    public void testDoesNotReturnStaleResponseIfClientExplicitlyRequestsFirstHandOneWithPragma()
            throws Exception {
        HttpRequest req = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        req.setHeader("Pragma","no-cache");
        testDoesNotReturnStaleResponseOnError(req);
    }

    @Test
    public void testDoesNotReturnStaleResponseIfClientExplicitlyRequestsFreshWithMaxAge()
            throws Exception {
        HttpRequest req = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        req.setHeader("Cache-Control","max-age=0");
        testDoesNotReturnStaleResponseOnError(req);
    }

    @Test
    public void testDoesNotReturnStaleResponseIfClientExplicitlySpecifiesLargerMaxAge()
            throws Exception {
        HttpRequest req = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        req.setHeader("Cache-Control","max-age=20");
        testDoesNotReturnStaleResponseOnError(req);
    }


    @Test
    public void testDoesNotReturnStaleResponseIfClientExplicitlyRequestsFreshWithMinFresh()
    throws Exception {
        HttpRequest req = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        req.setHeader("Cache-Control","min-fresh=2");

        testDoesNotReturnStaleResponseOnError(req);
    }

    @Test
    public void testDoesNotReturnStaleResponseIfClientExplicitlyRequestsFreshWithMaxStale()
    throws Exception {
        HttpRequest req = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        req.setHeader("Cache-Control","max-stale=2");

        testDoesNotReturnStaleResponseOnError(req);
    }

    @Test
    public void testMayReturnStaleResponseIfClientExplicitlySpecifiesAcceptableMaxStale()
            throws Exception {
        HttpRequest req1 = requestToPopulateStaleCacheEntry();
        HttpRequest req2 = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        req2.setHeader("Cache-Control","max-stale=20");

        backendExpectsAnyRequest().andThrow(new IOException()).times(0,1);

        replayMocks();
        impl.execute(host, req1);
        HttpResponse result = impl.execute(host, req2);
        verifyMocks();

        assertEquals(HttpStatus.SC_OK, result.getStatusLine().getStatusCode());
        assertNotNull(result.getFirstHeader("Warning"));
    }

    /*
     * "A correct cache MUST respond to a request with the most up-to-date
     * response held by the cache that is appropriate to the request
     * (see sections 13.2.5, 13.2.6, and 13.12) which meets one of the
     * following conditions:
     *
     * 1. It has been checked for equivalence with what the origin server
     * would have returned by revalidating the response with the
     * origin server (section 13.3);
     *
     * 2. It is "fresh enough" (see section 13.2). In the default case,
     * this means it meets the least restrictive freshness requirement
     * of the client, origin server, and cache (see section 14.9); if
     * the origin server so specifies, it is the freshness requirement
     * of the origin server alone.
     *
     * If a stored response is not "fresh enough" by the most
     * restrictive freshness requirement of both the client and the
     * origin server, in carefully considered circumstances the cache
     * MAY still return the response with the appropriate Warning
     * header (see section 13.1.5 and 14.46), unless such a response
     * is prohibited (e.g., by a "no-store" cache-directive, or by a
     * "no-cache" cache-request-directive; see section 14.9).
     *
     * 3. It is an appropriate 304 (Not Modified), 305 (Proxy Redirect),
     * or error (4xx or 5xx) response message.
     *
     * If the cache can not communicate with the origin server, then a
     * correct cache SHOULD respond as above if the response can be
     * correctly served from the cache..."
     *
     * http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html#sec13.1.1
     */
    @Test
    public void testReturnsCachedResponsesAppropriatelyWhenNoOriginCommunication()
        throws Exception {
        HttpRequest req1 = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpResponse resp1 = HttpTestUtils.make200Response();
        Date now = new Date();
        Date tenSecondsAgo = new Date(now.getTime() - 10 * 1000L);
        resp1.setHeader("Cache-Control", "public, max-age=5");
        resp1.setHeader("ETag","\"etag\"");
        resp1.setHeader("Date", DateUtils.formatDate(tenSecondsAgo));

        backendExpectsAnyRequest().andReturn(resp1);

        HttpRequest req2 = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);

        backendExpectsAnyRequest().andThrow(new IOException()).anyTimes();

        replayMocks();
        impl.execute(host, req1);
        HttpResponse result = impl.execute(host, req2);
        verifyMocks();

        assertEquals(HttpStatus.SC_OK, result.getStatusLine().getStatusCode());
        boolean warning111Found = false;
        for(Header h : result.getHeaders("Warning")) {
            for(WarningValue wv : WarningValue.getWarningValues(h)) {
                if (wv.getWarnCode() == 111) {
                    warning111Found = true;
                    break;
                }
            }
        }
        assertTrue(warning111Found);
    }

    /*
     * "If a cache receives a response (either an entire response, or a
     * 304 (Not Modified) response) that it would normally forward to the
     * requesting client, and the received response is no longer fresh,
     * the cache SHOULD forward it to the requesting client without adding
     * a new Warning (but without removing any existing Warning headers).
     * A cache SHOULD NOT attempt to revalidate a response simply because
     * that response became stale in transit; this might lead to an
     * infinite loop."
     *
     * http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html#sec13.1.1
     */
    @Test
    public void testDoesNotAddNewWarningHeaderIfResponseArrivesStale()
        throws Exception {
        Date now = new Date();
        Date tenSecondsAgo = new Date(now.getTime() - 10 * 1000L);
        originResponse.setHeader("Date", DateUtils.formatDate(tenSecondsAgo));
        originResponse.setHeader("Cache-Control","public, max-age=5");
        originResponse.setHeader("ETag","\"etag\"");

        backendExpectsAnyRequest().andReturn(originResponse);

        replayMocks();
        HttpResponse result = impl.execute(host, request);
        verifyMocks();

        assertNull(result.getFirstHeader("Warning"));
    }

    @Test
    public void testForwardsExistingWarningHeadersOnResponseThatArrivesStale()
        throws Exception {
        Date now = new Date();
        Date tenSecondsAgo = new Date(now.getTime() - 10 * 1000L);
        originResponse.setHeader("Date", DateUtils.formatDate(tenSecondsAgo));
        originResponse.setHeader("Cache-Control","public, max-age=5");
        originResponse.setHeader("ETag","\"etag\"");
        originResponse.addHeader("Age","10");
        final String warning = "110 fred \"Response is stale\"";
        originResponse.addHeader("Warning",warning);

        backendExpectsAnyRequest().andReturn(originResponse);

        replayMocks();
        HttpResponse result = impl.execute(host, request);
        verifyMocks();

        assertEquals(warning, result.getFirstHeader("Warning").getValue());
    }

    /*
     * "A transparent proxy SHOULD NOT modify an end-to-end header unless
     * the definition of that header requires or specifically allows that."
     *
     * http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html#sec13.5.2
     */
    private void testDoesNotModifyHeaderOnResponses(final String headerName)
            throws Exception {
        final String headerValue = HttpTestUtils
            .getCanonicalHeaderValue(originResponse, headerName);
        backendExpectsAnyRequest().andReturn(originResponse);
        replayMocks();
        HttpResponse result = impl.execute(host, request);
        verifyMocks();
        assertEquals(headerValue,
            result.getFirstHeader(headerName).getValue());
    }

    private void testDoesNotModifyHeaderOnRequests(final String headerName)
            throws Exception {
        final String headerValue = HttpTestUtils.getCanonicalHeaderValue(request, headerName);
        Capture<HttpRequest> cap = new Capture<HttpRequest>();
        EasyMock.expect(mockBackend.execute(EasyMock.isA(HttpHost.class),
                EasyMock.capture(cap), (HttpContext)EasyMock.isNull()))
                .andReturn(originResponse);
        replayMocks();
        impl.execute(host, request);
        verifyMocks();
        assertEquals(headerValue,
                HttpTestUtils.getCanonicalHeaderValue(cap.getValue(),
                        headerName));
    }

    @Test
    public void testDoesNotModifyAcceptRangesOnResponses()
    throws Exception {
        final String headerName = "Accept-Ranges";
        originResponse.setHeader(headerName,"bytes");
        testDoesNotModifyHeaderOnResponses(headerName);
    }

    @Test
    public void testDoesNotModifyAuthorizationOnRequests()
            throws Exception {
        request.setHeader("Authorization", "Basic dXNlcjpwYXNzd2Q=");
        testDoesNotModifyHeaderOnRequests("Authorization");
    }

    @Test
    public void testDoesNotModifyContentLengthOnRequests()
            throws Exception {
        HttpEntityEnclosingRequest post =
            new BasicHttpEntityEnclosingRequest("POST", "/", HttpVersion.HTTP_1_1);
        post.setEntity(HttpTestUtils.makeBody(128));
        post.setHeader("Content-Length","128");
        request = post;
        testDoesNotModifyHeaderOnRequests("Content-Length");
    }

    @Test
    public void testDoesNotModifyContentLengthOnResponses()
            throws Exception {
        originResponse.setEntity(HttpTestUtils.makeBody(128));
        originResponse.setHeader("Content-Length","128");
        testDoesNotModifyHeaderOnResponses("Content-Length");
    }

    @Test
    public void testDoesNotModifyContentMD5OnRequests()
            throws Exception {
        HttpEntityEnclosingRequest post =
            new BasicHttpEntityEnclosingRequest("POST", "/", HttpVersion.HTTP_1_1);
        post.setEntity(HttpTestUtils.makeBody(128));
        post.setHeader("Content-Length","128");
        post.setHeader("Content-MD5","Q2hlY2sgSW50ZWdyaXR5IQ==");
        request = post;
        testDoesNotModifyHeaderOnRequests("Content-MD5");
    }

    @Test
    public void testDoesNotModifyContentMD5OnResponses()
            throws Exception {
        originResponse.setEntity(HttpTestUtils.makeBody(128));
        originResponse.setHeader("Content-MD5","Q2hlY2sgSW50ZWdyaXR5IQ==");
        testDoesNotModifyHeaderOnResponses("Content-MD5");
    }

    @Test
    public void testDoesNotModifyContentRangeOnRequests()
            throws Exception {
        HttpEntityEnclosingRequest put =
            new BasicHttpEntityEnclosingRequest("PUT", "/", HttpVersion.HTTP_1_1);
        put.setEntity(HttpTestUtils.makeBody(128));
        put.setHeader("Content-Length","128");
        put.setHeader("Content-Range","bytes 0-127/256");
        request = put;
        testDoesNotModifyHeaderOnRequests("Content-Range");
    }

    @Test
    public void testDoesNotModifyContentRangeOnResponses()
            throws Exception {
        request.setHeader("Range","bytes=0-128");
        originResponse.setStatusCode(HttpStatus.SC_PARTIAL_CONTENT);
        originResponse.setReasonPhrase("Partial Content");
        originResponse.setEntity(HttpTestUtils.makeBody(128));
        originResponse.setHeader("Content-Range","bytes 0-127/256");
        testDoesNotModifyHeaderOnResponses("Content-Range");
    }

    @Test
    public void testDoesNotModifyContentTypeOnRequests()
            throws Exception {
        HttpEntityEnclosingRequest post =
            new BasicHttpEntityEnclosingRequest("POST", "/", HttpVersion.HTTP_1_1);
        post.setEntity(HttpTestUtils.makeBody(128));
        post.setHeader("Content-Length","128");
        post.setHeader("Content-Type","application/octet-stream");
        request = post;
        testDoesNotModifyHeaderOnRequests("Content-Type");
    }

    @Test
    public void testDoesNotModifyContentTypeOnResponses()
            throws Exception {
        originResponse.setHeader("Content-Type","application/octet-stream");
        testDoesNotModifyHeaderOnResponses("Content-Type");
    }

    @Test
    public void testDoesNotModifyDateOnRequests()
        throws Exception {
        request.setHeader("Date", DateUtils.formatDate(new Date()));
        testDoesNotModifyHeaderOnRequests("Date");
    }

    @Test
    public void testDoesNotModifyDateOnResponses()
        throws Exception {
        originResponse.setHeader("Date", DateUtils.formatDate(new Date()));
        testDoesNotModifyHeaderOnResponses("Date");
    }

    @Test
    public void testDoesNotModifyETagOnResponses()
        throws Exception {
        originResponse.setHeader("ETag", "\"random-etag\"");
        testDoesNotModifyHeaderOnResponses("ETag");
    }

    @Test
    public void testDoesNotModifyExpiresOnResponses()
        throws Exception {
        originResponse.setHeader("Expires", DateUtils.formatDate(new Date()));
        testDoesNotModifyHeaderOnResponses("Expires");
    }

    @Test
    public void testDoesNotModifyFromOnRequests()
        throws Exception {
        request.setHeader("From", "foo@example.com");
        testDoesNotModifyHeaderOnRequests("From");
    }

    @Test
    public void testDoesNotModifyIfMatchOnRequests()
        throws Exception {
        request = new BasicHttpRequest("DELETE", "/", HttpVersion.HTTP_1_1);
        request.setHeader("If-Match", "\"etag\"");
        testDoesNotModifyHeaderOnRequests("If-Match");
    }

    @Test
    public void testDoesNotModifyIfModifiedSinceOnRequests()
        throws Exception {
        request.setHeader("If-Modified-Since", DateUtils.formatDate(new Date()));
        testDoesNotModifyHeaderOnRequests("If-Modified-Since");
    }

    @Test
    public void testDoesNotModifyIfNoneMatchOnRequests()
        throws Exception {
        request.setHeader("If-None-Match", "\"etag\"");
        testDoesNotModifyHeaderOnRequests("If-None-Match");
    }

    @Test
    public void testDoesNotModifyIfRangeOnRequests()
        throws Exception {
        request.setHeader("Range","bytes=0-128");
        request.setHeader("If-Range", "\"etag\"");
        testDoesNotModifyHeaderOnRequests("If-Range");
    }

    @Test
    public void testDoesNotModifyIfUnmodifiedSinceOnRequests()
        throws Exception {
        request = new BasicHttpRequest("DELETE", "/", HttpVersion.HTTP_1_1);
        request.setHeader("If-Unmodified-Since", DateUtils.formatDate(new Date()));
        testDoesNotModifyHeaderOnRequests("If-Unmodified-Since");
    }

    @Test
    public void testDoesNotModifyLastModifiedOnResponses()
        throws Exception {
        originResponse.setHeader("Last-Modified", DateUtils.formatDate(new Date()));
        testDoesNotModifyHeaderOnResponses("Last-Modified");
    }

    @Test
    public void testDoesNotModifyLocationOnResponses()
        throws Exception {
        originResponse.setStatusCode(HttpStatus.SC_TEMPORARY_REDIRECT);
        originResponse.setReasonPhrase("Temporary Redirect");
        originResponse.setHeader("Location", "http://foo.example.com/bar");
        testDoesNotModifyHeaderOnResponses("Location");
    }

    @Test
    public void testDoesNotModifyRangeOnRequests()
        throws Exception {
        request.setHeader("Range", "bytes=0-128");
        testDoesNotModifyHeaderOnRequests("Range");
    }

    @Test
    public void testDoesNotModifyRefererOnRequests()
        throws Exception {
        request.setHeader("Referer", "http://foo.example.com/bar");
        testDoesNotModifyHeaderOnRequests("Referer");
    }

    @Test
    public void testDoesNotModifyRetryAfterOnResponses()
        throws Exception {
        originResponse.setStatusCode(HttpStatus.SC_SERVICE_UNAVAILABLE);
        originResponse.setReasonPhrase("Service Unavailable");
        originResponse.setHeader("Retry-After", "120");
        testDoesNotModifyHeaderOnResponses("Retry-After");
    }

    @Test
    public void testDoesNotModifyServerOnResponses()
        throws Exception {
        originResponse.setHeader("Server", "SomeServer/1.0");
        testDoesNotModifyHeaderOnResponses("Server");
    }

    @Test
    public void testDoesNotModifyUserAgentOnRequests()
        throws Exception {
        request.setHeader("User-Agent", "MyClient/1.0");
        testDoesNotModifyHeaderOnRequests("User-Agent");
    }

    @Test
    public void testDoesNotModifyVaryOnResponses()
        throws Exception {
        request.setHeader("Accept-Encoding","identity");
        originResponse.setHeader("Vary", "Accept-Encoding");
        testDoesNotModifyHeaderOnResponses("Vary");
    }

    @Test
    public void testDoesNotModifyExtensionHeaderOnRequests()
        throws Exception {
        request.setHeader("X-Extension","x-value");
        testDoesNotModifyHeaderOnRequests("X-Extension");
    }

    @Test
    public void testDoesNotModifyExtensionHeaderOnResponses()
        throws Exception {
        originResponse.setHeader("X-Extension", "x-value");
        testDoesNotModifyHeaderOnResponses("X-Extension");
    }


    /*
     * "[HTTP/1.1 clients], If only a Last-Modified value has been provided
     * by the origin server, SHOULD use that value in non-subrange cache-
     * conditional requests (using If-Modified-Since)."
     *
     * http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html#sec13.3.4
     */
    @Test
    public void testUsesLastModifiedDateForCacheConditionalRequests()
            throws Exception {
        Date now = new Date();
        Date tenSecondsAgo = new Date(now.getTime() - 10 * 1000L);
        Date twentySecondsAgo = new Date(now.getTime() - 20 * 1000L);
        final String lmDate = DateUtils.formatDate(twentySecondsAgo);

        HttpRequest req1 =
            new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpResponse resp1 = HttpTestUtils.make200Response();
        resp1.setHeader("Date", DateUtils.formatDate(tenSecondsAgo));
        resp1.setHeader("Last-Modified", lmDate);
        resp1.setHeader("Cache-Control","max-age=5");

        backendExpectsAnyRequest().andReturn(resp1);

        Capture<HttpRequest> cap = new Capture<HttpRequest>();
        HttpRequest req2 =
            new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpResponse resp2 = HttpTestUtils.make200Response();

        EasyMock.expect(mockBackend.execute(EasyMock.same(host),
                EasyMock.capture(cap), (HttpContext)EasyMock.isNull()))
            .andReturn(resp2);

        replayMocks();
        impl.execute(host, req1);
        impl.execute(host, req2);
        verifyMocks();

        HttpRequest captured = cap.getValue();
        Header ifModifiedSince =
            captured.getFirstHeader("If-Modified-Since");
        assertEquals(lmDate, ifModifiedSince.getValue());
    }

    /*
     * "[HTTP/1.1 clients], if both an entity tag and a Last-Modified value
     * have been provided by the origin server, SHOULD use both validators
     * in cache-conditional requests. This allows both HTTP/1.0 and
     * HTTP/1.1 caches to respond appropriately."
     *
     * http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html#sec13.3.4
     */
    @Test
    public void testUsesBothLastModifiedAndETagForConditionalRequestsIfAvailable()
            throws Exception {
        Date now = new Date();
        Date tenSecondsAgo = new Date(now.getTime() - 10 * 1000L);
        Date twentySecondsAgo = new Date(now.getTime() - 20 * 1000L);
        final String lmDate = DateUtils.formatDate(twentySecondsAgo);
        final String etag = "\"etag\"";

        HttpRequest req1 =
            new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpResponse resp1 = HttpTestUtils.make200Response();
        resp1.setHeader("Date", DateUtils.formatDate(tenSecondsAgo));
        resp1.setHeader("Last-Modified", lmDate);
        resp1.setHeader("Cache-Control","max-age=5");
        resp1.setHeader("ETag", etag);

        backendExpectsAnyRequest().andReturn(resp1);

        Capture<HttpRequest> cap = new Capture<HttpRequest>();
        HttpRequest req2 =
            new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpResponse resp2 = HttpTestUtils.make200Response();

        EasyMock.expect(mockBackend.execute(EasyMock.same(host),
                EasyMock.capture(cap), (HttpContext)EasyMock.isNull()))
            .andReturn(resp2);

        replayMocks();
        impl.execute(host, req1);
        impl.execute(host, req2);
        verifyMocks();

        HttpRequest captured = cap.getValue();
        Header ifModifiedSince =
            captured.getFirstHeader("If-Modified-Since");
        assertEquals(lmDate, ifModifiedSince.getValue());
        Header ifNoneMatch =
            captured.getFirstHeader("If-None-Match");
        assertEquals(etag, ifNoneMatch.getValue());
    }

    /*
     * "If an origin server wishes to force a semantically transparent cache
     * to validate every request, it MAY assign an explicit expiration time
     * in the past. This means that the response is always stale, and so the
     * cache SHOULD validate it before using it for subsequent requests."
     *
     * http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html#sec13.2.1
     */
    @Test
    public void testRevalidatesCachedResponseWithExpirationInThePast()
            throws Exception {
        Date now = new Date();
        Date oneSecondAgo = new Date(now.getTime() - 1 * 1000L);
        Date oneSecondFromNow = new Date(now.getTime() + 1 * 1000L);
        Date twoSecondsFromNow = new Date(now.getTime() + 2 * 1000L);
        HttpRequest req1 =
            new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpResponse resp1 = HttpTestUtils.make200Response();
        resp1.setHeader("ETag","\"etag\"");
        resp1.setHeader("Date", DateUtils.formatDate(now));
        resp1.setHeader("Expires",DateUtils.formatDate(oneSecondAgo));

        backendExpectsAnyRequest().andReturn(resp1);

        HttpRequest req2 =
            new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpRequest revalidate =
            new BasicHttpRequest("GET", "/",HttpVersion.HTTP_1_1);
        revalidate.setHeader("If-None-Match","\"etag\"");

        HttpResponse resp2 = new BasicHttpResponse(HttpVersion.HTTP_1_1,
                HttpStatus.SC_NOT_MODIFIED, "Not Modified");
        resp2.setHeader("Date", DateUtils.formatDate(twoSecondsFromNow));
        resp2.setHeader("Expires", DateUtils.formatDate(oneSecondFromNow));
        resp2.setHeader("ETag","\"etag\"");

        expect(mockBackend.execute(isA(HttpHost.class),
                eqRequest(revalidate), (HttpContext)isNull()))
            .andReturn(resp2);

        replayMocks();
        impl.execute(host, req1);
        HttpResponse result = impl.execute(host, req2);
        verifyMocks();

        assertEquals(HttpStatus.SC_OK,
                result.getStatusLine().getStatusCode());
    }

    /* "When a client tries to revalidate a cache entry, and the response
     * it receives contains a Date header that appears to be older than the
     * one for the existing entry, then the client SHOULD repeat the
     * request unconditionally, and include
     *     Cache-Control: max-age=0
     * to force any intermediate caches to validate their copies directly
     * with the origin server, or
     *     Cache-Control: no-cache
     * to force any intermediate caches to obtain a new copy from the
     * origin server."
     *
     * http://www.w3.org/Protocols/rfc2616/rfc2616-sec13.html#sec13.2.6
     */
    @Test
    public void testRetriesValidationThatResultsInAnOlderDated304Response()
        throws Exception {
        Date now = new Date();
        Date tenSecondsAgo = new Date(now.getTime() - 10 * 1000L);
        Date elevenSecondsAgo = new Date(now.getTime() - 11 * 1000L);
        HttpRequest req1 =
            new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpResponse resp1 = HttpTestUtils.make200Response();
        resp1.setHeader("ETag","\"etag\"");
        resp1.setHeader("Date", DateUtils.formatDate(tenSecondsAgo));
        resp1.setHeader("Cache-Control","max-age=5");

        backendExpectsAnyRequest().andReturn(resp1);

        HttpRequest req2 = new BasicHttpRequest("GET", "/", HttpVersion.HTTP_1_1);
        HttpResponse resp2 = new BasicHttpResponse(HttpVersion.HTTP_1_1,
                HttpStatus.SC_NOT_MODIFIED, "Not Modified");
        resp2.setHeader("ETag","\"etag\"");
        resp2.setHeader("Date", DateUtils.formatDate(elevenSecondsAgo));

        backendExpectsAnyRequest().andReturn(resp2);

        Capture<HttpRequest> cap = new Capture<HttpRequest>();
        HttpResponse resp3 = HttpTestUtils.make200Response();
        resp3.setHeader("ETag","\"etag2\"");
        resp3.setHeader("Date", DateUtils.formatDate(now));
        resp3.setHeader("Cache-Control","max-age=5");

        expect(mockBackend.execute(isA(HttpHost.class), capture(cap),
                (HttpContext)isNull()))
            .andReturn(resp3);

        replayMocks();
        impl.execute(host, req1);
        impl.execute(host, req2);
        verifyMocks();

        HttpRequest captured = cap.getValue();
        boolean hasMaxAge0 = false;
        boolean hasNoCache = false;
        for(Header h : captured.getHeaders("Cache-Control")) {
            for(HeaderElement elt : h.getElements()) {
                if ("max-age".equals(elt.getName())) {
                    try {
                        int maxage = Integer.parseInt(elt.getValue());
                        if (maxage == 0) {
                            hasMaxAge0 = true;
                        }
                    } catch (NumberFormatException nfe) {
                        // nop
                    }
                } else if ("no-cache".equals(elt.getName())) {
                    hasNoCache = true;
                }
            }
        }
        assertTrue(hasMaxAge0 || hasNoCache);
        assertNull(captured.getFirstHeader("If-None-Match"));
        assertNull(captured.getFirstHeader("If-Modified-Since"));
        assertNull(captured.getFirstHeader("If-Range"));
        assertNull(captured.getFirstHeader("If-Match"));
        assertNull(captured.getFirstHeader("If-Unmodified-Since"));
    }
}
