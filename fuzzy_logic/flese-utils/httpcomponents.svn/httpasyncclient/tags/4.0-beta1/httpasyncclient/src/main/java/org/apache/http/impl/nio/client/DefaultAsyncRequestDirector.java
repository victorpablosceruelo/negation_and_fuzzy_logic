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
package org.apache.http.impl.nio.client;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.ProtocolException;
import org.apache.http.ProtocolVersion;
import org.apache.http.auth.AuthProtocolState;
import org.apache.http.auth.AuthScheme;
import org.apache.http.auth.AuthState;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.AuthenticationStrategy;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.NonRepeatableRequestException;
import org.apache.http.client.RedirectException;
import org.apache.http.client.RedirectStrategy;
import org.apache.http.client.UserTokenHandler;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.client.params.ClientPNames;
import org.apache.http.client.params.HttpClientParams;
import org.apache.http.client.protocol.ClientContext;
import org.apache.http.client.utils.URIUtils;
import org.apache.http.concurrent.FutureCallback;
import org.apache.http.conn.ConnectionKeepAliveStrategy;
import org.apache.http.conn.routing.BasicRouteDirector;
import org.apache.http.conn.routing.HttpRoute;
import org.apache.http.conn.routing.HttpRouteDirector;
import org.apache.http.conn.routing.HttpRoutePlanner;
import org.apache.http.impl.auth.BasicScheme;
import org.apache.http.impl.client.ClientParamsStack;
import org.apache.http.impl.client.EntityEnclosingRequestWrapper;
import org.apache.http.impl.client.HttpAuthenticator;
import org.apache.http.impl.client.RequestWrapper;
import org.apache.http.impl.client.RoutedRequest;
import org.apache.http.message.BasicHttpRequest;
import org.apache.http.nio.ContentDecoder;
import org.apache.http.nio.ContentEncoder;
import org.apache.http.nio.IOControl;
import org.apache.http.nio.conn.ClientAsyncConnectionManager;
import org.apache.http.nio.conn.ManagedClientAsyncConnection;
import org.apache.http.nio.conn.scheme.AsyncScheme;
import org.apache.http.nio.protocol.HttpAsyncRequestExecutionHandler;
import org.apache.http.nio.protocol.HttpAsyncRequestExecutor;
import org.apache.http.nio.protocol.HttpAsyncRequestProducer;
import org.apache.http.nio.protocol.HttpAsyncResponseConsumer;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.protocol.ExecutionContext;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpProcessor;

class DefaultAsyncRequestDirector<T> implements HttpAsyncRequestExecutionHandler<T> {

    private final Log log;

    private final HttpAsyncRequestProducer requestProducer;
    private final HttpAsyncResponseConsumer<T> responseConsumer;
    private final HttpContext localContext;
    private final ResultCallback<T> resultCallback;
    private final ClientAsyncConnectionManager connmgr;
    private final HttpProcessor httppocessor;
    private final HttpRoutePlanner routePlanner;
    private final HttpRouteDirector routeDirector;
    private final ConnectionReuseStrategy reuseStrategy;
    private final ConnectionKeepAliveStrategy keepaliveStrategy;
    private final RedirectStrategy redirectStrategy;
    private final AuthenticationStrategy targetAuthStrategy;
    private final AuthenticationStrategy proxyAuthStrategy;
    private final UserTokenHandler userTokenHandler;
    private final AuthState targetAuthState;
    private final AuthState proxyAuthState;
    private final HttpAuthenticator authenticator;
    private final HttpParams clientParams;

    private RoutedRequest mainRequest;
    private RoutedRequest followup;
    private HttpResponse finalResponse;

    private ClientParamsStack params;
    private RequestWrapper currentRequest;
    private HttpResponse currentResponse;
    private boolean routeEstablished;
    private Future<ManagedClientAsyncConnection> connFuture;
    private ManagedClientAsyncConnection managedConn;
    private int redirectCount;
    private ByteBuffer tmpbuf;
    private boolean requestContentProduced;
    private int execCount;

    public DefaultAsyncRequestDirector(
            final Log log,
            final HttpAsyncRequestProducer requestProducer,
            final HttpAsyncResponseConsumer<T> responseConsumer,
            final HttpContext localContext,
            final ResultCallback<T> callback,
            final ClientAsyncConnectionManager connmgr,
            final HttpProcessor httppocessor,
            final HttpRoutePlanner routePlanner,
            final ConnectionReuseStrategy reuseStrategy,
            final ConnectionKeepAliveStrategy keepaliveStrategy,
            final RedirectStrategy redirectStrategy,
            final AuthenticationStrategy targetAuthStrategy,
            final AuthenticationStrategy proxyAuthStrategy,
            final UserTokenHandler userTokenHandler,
            final HttpParams clientParams) {
        super();
        this.log = log;
        this.requestProducer = requestProducer;
        this.responseConsumer = responseConsumer;
        this.localContext = localContext;
        this.resultCallback = callback;
        this.connmgr = connmgr;
        this.httppocessor = httppocessor;
        this.routePlanner = routePlanner;
        this.reuseStrategy = reuseStrategy;
        this.keepaliveStrategy = keepaliveStrategy;
        this.redirectStrategy = redirectStrategy;
        this.routeDirector = new BasicRouteDirector();
        this.targetAuthStrategy = targetAuthStrategy;
        this.proxyAuthStrategy = proxyAuthStrategy;
        this.userTokenHandler = userTokenHandler;
        this.targetAuthState = new AuthState();
        this.proxyAuthState = new AuthState();
        this.authenticator     = new HttpAuthenticator(log);
        this.clientParams = clientParams;
    }

    public synchronized void close() throws IOException {
        releaseResources();
    }

    public synchronized void start() {
        try {

            this.localContext.setAttribute(ClientContext.TARGET_AUTH_STATE, this.targetAuthState);
            this.localContext.setAttribute(ClientContext.PROXY_AUTH_STATE, this.proxyAuthState);

            HttpHost target = this.requestProducer.getTarget();
            HttpRequest request = this.requestProducer.generateRequest();
            this.params = new ClientParamsStack(null, this.clientParams, request.getParams(), null);
            RequestWrapper wrapper = wrapRequest(request);
            wrapper.setParams(this.params);
            HttpRoute route = determineRoute(target, wrapper, this.localContext);
            this.mainRequest = new RoutedRequest(wrapper, route);
            this.requestContentProduced = false;
            requestConnection();
        } catch (Exception ex) {
            failed(ex);
        }
    }

    public HttpHost getTarget() {
        return this.requestProducer.getTarget();
    }

    public synchronized HttpRequest generateRequest() throws IOException, HttpException {
        HttpRoute route = this.mainRequest.getRoute();
        if (!this.routeEstablished) {
            int step;
            do {
                HttpRoute fact = this.managedConn.getRoute();
                step = this.routeDirector.nextStep(route, fact);
                switch (step) {
                case HttpRouteDirector.CONNECT_TARGET:
                case HttpRouteDirector.CONNECT_PROXY:
                    break;
                case HttpRouteDirector.TUNNEL_TARGET:
                    this.log.debug("Tunnel required");
                    HttpRequest connect = createConnectRequest(route);
                    this.currentRequest = wrapRequest(connect);
                    this.currentRequest.setParams(this.params);
                    break;
                case HttpRouteDirector.TUNNEL_PROXY:
                    throw new HttpException("Proxy chains are not supported");
                case HttpRouteDirector.LAYER_PROTOCOL:
                    managedConn.layerProtocol(this.localContext, this.params);
                    break;
                case HttpRouteDirector.UNREACHABLE:
                    throw new HttpException("Unable to establish route: " +
                            "planned = " + route + "; current = " + fact);
                case HttpRouteDirector.COMPLETE:
                    this.routeEstablished = true;
                    break;
                default:
                    throw new IllegalStateException("Unknown step indicator "
                            + step + " from RouteDirector.");
                }
            } while (step > HttpRouteDirector.COMPLETE && this.currentRequest == null);
        }

        HttpHost target = (HttpHost) this.params.getParameter(ClientPNames.VIRTUAL_HOST);
        if (target == null) {
            target = route.getTargetHost();
        }
        HttpHost proxy = route.getProxyHost();
        this.localContext.setAttribute(ExecutionContext.HTTP_TARGET_HOST, target);
        this.localContext.setAttribute(ExecutionContext.HTTP_PROXY_HOST, proxy);
        this.localContext.setAttribute(ExecutionContext.HTTP_CONNECTION, this.managedConn);

        if (this.currentRequest == null) {
            this.currentRequest = this.mainRequest.getRequest();

            String userinfo = this.currentRequest.getURI().getUserInfo();
            if (userinfo != null) {
                this.targetAuthState.update(
                        new BasicScheme(), new UsernamePasswordCredentials(userinfo));
            }
            
            // Re-write request URI if needed
            rewriteRequestURI(this.currentRequest, route);
        }
        // Reset headers on the request wrapper
        this.currentRequest.resetHeaders();

        this.currentRequest.incrementExecCount();
        if (this.currentRequest.getExecCount() > 1
                && !this.requestProducer.isRepeatable()
                && this.requestContentProduced) {
            throw new NonRepeatableRequestException("Cannot retry request " +
                "with a non-repeatable request entity.");
        }
        this.execCount++;
        if (this.log.isDebugEnabled()) {
            this.log.debug("Attempt " + this.execCount + " to execute request");
        }
        return this.currentRequest;
    }

    public synchronized void produceContent(
            final ContentEncoder encoder, final IOControl ioctrl) throws IOException {
        this.requestContentProduced = true;
        this.requestProducer.produceContent(encoder, ioctrl);
        if (encoder.isCompleted()) {
            this.requestProducer.resetRequest();
        }
    }

    public void requestCompleted(final HttpContext context) {
        this.requestProducer.requestCompleted(context);
    }

    public boolean isRepeatable() {
        return this.requestProducer.isRepeatable();
    }

    public void resetRequest() throws IOException {
        this.requestProducer.resetRequest();
    }

    public synchronized void responseReceived(
            final HttpResponse response) throws IOException, HttpException {
        if (this.log.isDebugEnabled()) {
            this.log.debug("Response: " + response.getStatusLine());
        }
        this.currentResponse = response;
        this.currentResponse.setParams(this.params);

        int status = this.currentResponse.getStatusLine().getStatusCode();

        if (!this.routeEstablished) {
            String method = this.currentRequest.getMethod();
            if (method.equalsIgnoreCase("CONNECT") && status == HttpStatus.SC_OK) {
                this.managedConn.tunnelTarget(this.params);
            } else {
                this.followup = handleConnectResponse();
                if (this.followup == null) {
                    this.finalResponse = response;
                }
            }
        } else {
            this.followup = handleResponse();
            if (this.followup == null) {
                this.finalResponse = response;
            }

            Object userToken = this.localContext.getAttribute(ClientContext.USER_TOKEN);
            if (managedConn != null && userToken == null) {
                userToken = userTokenHandler.getUserToken(this.localContext);
                if (userToken != null) {
                    this.localContext.setAttribute(ClientContext.USER_TOKEN, userToken);
                    managedConn.setState(userToken);
                }
            }
        }
        if (this.finalResponse != null) {
            this.responseConsumer.responseReceived(response);
        }
    }

    public synchronized void consumeContent(
            final ContentDecoder decoder, final IOControl ioctrl) throws IOException {
        if (this.finalResponse != null) {
            this.responseConsumer.consumeContent(decoder, ioctrl);
        } else {
            if (this.tmpbuf == null) {
                this.tmpbuf = ByteBuffer.allocate(2048);
            }
            this.tmpbuf.clear();
            decoder.read(this.tmpbuf);
        }
    }

    private void releaseConnection() {
        if (this.managedConn != null) {
            try {
                this.managedConn.getContext().removeAttribute(HttpAsyncRequestExecutor.HTTP_HANDLER);
                this.managedConn.releaseConnection();
            } catch (IOException ioex) {
                this.log.debug("I/O error releasing connection", ioex);
            }
            this.managedConn = null;
        }
    }

    private void releaseResources() {
        if (this.managedConn != null) {
            try {
                this.managedConn.abortConnection();
            } catch (IOException ioex) {
                this.log.debug("I/O error releasing connection", ioex);
            }
            this.managedConn = null;
        }
        if (this.connFuture != null) {
            this.connFuture.cancel(true);
            this.connFuture = null;
        }
        try {
            this.requestProducer.close();
        } catch (IOException ex) {
            this.log.debug("I/O error closing request producer", ex);
        }
        try {
            this.responseConsumer.close();
        } catch (IOException ex) {
            this.log.debug("I/O error closing response consumer", ex);
        }
    }

    public synchronized void failed(final Exception ex) {
        try {
            this.responseConsumer.failed(ex);
        } finally {
            try {
                this.resultCallback.failed(ex, this);
            } finally {
                releaseResources();
            }
        }
    }

    public synchronized void responseCompleted(final HttpContext context) {
        this.log.debug("Response fully read");
        try {
            if (this.resultCallback.isDone()) {
                return;
            }
            if (this.managedConn.isOpen()) {
                long duration = this.keepaliveStrategy.getKeepAliveDuration(
                        this.currentResponse, this.localContext);
                if (this.log.isDebugEnabled()) {
                    String s;
                    if (duration > 0) {
                        s = "for " + duration + " " + TimeUnit.MILLISECONDS;
                    } else {
                        s = "indefinitely";
                    }
                    this.log.debug("Connection can be kept alive " + s);
                }
                this.managedConn.setIdleDuration(duration, TimeUnit.MILLISECONDS);
            } else {
                this.log.debug("Connection cannot be kept alive");
                this.managedConn.unmarkReusable();
                if (this.proxyAuthState.getState() == AuthProtocolState.SUCCESS
                        && this.proxyAuthState.getAuthScheme() != null
                        && this.proxyAuthState.getAuthScheme().isConnectionBased()) {
                    this.log.debug("Resetting proxy auth state");
                    this.proxyAuthState.reset();
                }
                if (this.targetAuthState.getState() == AuthProtocolState.SUCCESS
                        && this.targetAuthState.getAuthScheme() != null
                        && this.targetAuthState.getAuthScheme().isConnectionBased()) {
                    this.log.debug("Resetting target auth state");
                    this.targetAuthState.reset();
                }
            }

            if (this.finalResponse != null) {
                this.responseConsumer.responseCompleted(this.localContext);
                this.log.debug("Response processed");
                releaseConnection();
                T result = this.responseConsumer.getResult();
                Exception ex = this.responseConsumer.getException();
                if (ex == null) {
                    this.resultCallback.completed(result, this);
                } else {
                    this.resultCallback.failed(ex, this);
                }
            } else {
                if (this.followup != null) {
                    HttpRoute actualRoute = this.mainRequest.getRoute();
                    HttpRoute newRoute = this.followup.getRoute();
                    if (!actualRoute.equals(newRoute)) {
                        releaseConnection();
                    }
                    this.mainRequest = this.followup;
                }
                if (this.managedConn != null && !this.managedConn.isOpen()) {
                    releaseConnection();
                }
                if (this.managedConn != null) {
                    this.managedConn.requestOutput();
                } else {
                    requestConnection();
                }
            }
            this.followup = null;
            this.currentRequest = null;
            this.currentResponse = null;
        } catch (RuntimeException runex) {
            failed(runex);
            throw runex;
        }
    }

    public synchronized boolean cancel() {
        this.log.debug("HTTP exchange cancelled");
        try {
            boolean cancelled = this.responseConsumer.cancel();

            T result = this.responseConsumer.getResult();
            Exception ex = this.responseConsumer.getException();
            if (ex != null) {
                this.resultCallback.failed(ex, this);
            } else if (result != null) {
                this.resultCallback.completed(result, this);
            } else {
                this.resultCallback.cancelled(this);
            }
            return cancelled;
        } catch (RuntimeException runex) {
            this.resultCallback.failed(runex, this);
            throw runex;
        } finally {
            releaseResources();
        }
    }

    public boolean isDone() {
        return this.resultCallback.isDone();
    }

    public T getResult() {
        return this.responseConsumer.getResult();
    }

    public Exception getException() {
        return this.responseConsumer.getException();
    }

    private synchronized void connectionRequestCompleted(final ManagedClientAsyncConnection conn) {
        if (this.log.isDebugEnabled()) {
            this.log.debug("Connection request suceeded: " + conn);
        }
        try {
            HttpRoute route = this.mainRequest.getRoute();
            if (!conn.isOpen()) {
                conn.open(route, this.localContext, this.params);
            }
            this.managedConn = conn;
            this.managedConn.getContext().setAttribute(HttpAsyncRequestExecutor.HTTP_HANDLER, this);
            this.managedConn.requestOutput();
            this.routeEstablished = route.equals(conn.getRoute());
        } catch (IOException ex) {
            failed(ex);
        } catch (RuntimeException runex) {
            failed(runex);
            throw runex;
        }
    }

    private synchronized void connectionRequestFailed(final Exception ex) {
        this.log.debug("Connection request failed", ex);
        try {
            this.resultCallback.failed(ex, this);
        } finally {
            releaseResources();
        }
    }

    private synchronized void connectionRequestCancelled() {
        this.log.debug("Connection request cancelled");
        try {
            this.resultCallback.cancelled(this);
        } finally {
            releaseResources();
        }
    }

    class InternalFutureCallback implements FutureCallback<ManagedClientAsyncConnection> {

        public void completed(final ManagedClientAsyncConnection session) {
            connectionRequestCompleted(session);
        }

        public void failed(final Exception ex) {
            connectionRequestFailed(ex);
        }

        public void cancelled() {
            connectionRequestCancelled();
        }

    }

    private void requestConnection() {
        HttpRoute route = this.mainRequest.getRoute();
        long connectTimeout = HttpConnectionParams.getConnectionTimeout(this.params);
        Object userToken = this.localContext.getAttribute(ClientContext.USER_TOKEN);
        this.connFuture = this.connmgr.leaseConnection(
                route, userToken,
                connectTimeout, TimeUnit.MILLISECONDS,
                new InternalFutureCallback());
    }

    protected HttpRoute determineRoute(
            HttpHost target,
            final HttpRequest request,
            final HttpContext context) throws HttpException {
        if (target == null) {
            target = (HttpHost) request.getParams().getParameter(ClientPNames.DEFAULT_HOST);
        }
        if (target == null) {
            throw new IllegalStateException("Target host could not be resolved");
        }
        return this.routePlanner.determineRoute(target, request, context);
    }

    private RequestWrapper wrapRequest(final HttpRequest request) throws ProtocolException {
        if (request instanceof HttpEntityEnclosingRequest) {
            return new EntityEnclosingRequestWrapper((HttpEntityEnclosingRequest) request);
        } else {
            return new RequestWrapper(request);
        }
    }

    protected void rewriteRequestURI(
            final RequestWrapper request, final HttpRoute route) throws ProtocolException {
        try {
            URI uri = request.getURI();
            if (route.getProxyHost() != null && !route.isTunnelled()) {
                // Make sure the request URI is absolute
                if (!uri.isAbsolute()) {
                    HttpHost target = route.getTargetHost();
                    uri = URIUtils.rewriteURI(uri, target);
                    request.setURI(uri);
                }
            } else {
                // Make sure the request URI is relative
                if (uri.isAbsolute()) {
                    uri = URIUtils.rewriteURI(uri, null);
                    request.setURI(uri);
                }
            }
        } catch (URISyntaxException ex) {
            throw new ProtocolException("Invalid URI: " + request.getRequestLine().getUri(), ex);
        }
    }

    private HttpRequest createConnectRequest(final HttpRoute route) {
        // see RFC 2817, section 5.2 and
        // INTERNET-DRAFT: Tunneling TCP based protocols through
        // Web proxy servers
        HttpHost target = route.getTargetHost();
        String host = target.getHostName();
        int port = target.getPort();
        if (port < 0) {
            AsyncScheme scheme = this.connmgr.getSchemeRegistry().getScheme(target.getSchemeName());
            port = scheme.getDefaultPort();
        }
        StringBuilder buffer = new StringBuilder(host.length() + 6);
        buffer.append(host);
        buffer.append(':');
        buffer.append(Integer.toString(port));
        ProtocolVersion ver = HttpProtocolParams.getVersion(this.params);
        HttpRequest req = new BasicHttpRequest("CONNECT", buffer.toString(), ver);
        return req;
    }

    private RoutedRequest handleResponse() throws HttpException {
        RoutedRequest followup = null;
        if (HttpClientParams.isRedirecting(this.params)) {
            followup = handleRedirect();
            if (followup != null) {
                return followup;
            }
        }
        if (HttpClientParams.isAuthenticating(this.params)) {
            CredentialsProvider credsProvider = (CredentialsProvider) this.localContext.getAttribute(
                    ClientContext.CREDS_PROVIDER);
            if (credsProvider != null) {
                followup = handleTargetChallenge(credsProvider);
                if (followup != null) {
                    return followup;
                }
                followup = handleProxyChallenge(credsProvider);
                if (followup != null) {
                    return followup;
                }
            }
        }
        return null;
    }

    private RoutedRequest handleConnectResponse() throws HttpException {
        RoutedRequest followup = null;
        if (HttpClientParams.isAuthenticating(this.params)) {
            CredentialsProvider credsProvider = (CredentialsProvider) this.localContext.getAttribute(
                    ClientContext.CREDS_PROVIDER);
            if (credsProvider != null) {
                followup = handleProxyChallenge(credsProvider);
                if (followup != null) {
                    return followup;
                }
            }
        }
        return null;
    }

    private RoutedRequest handleRedirect() throws HttpException {
        if (this.redirectStrategy.isRedirected(
                this.currentRequest, this.currentResponse, this.localContext)) {

            HttpRoute route = this.mainRequest.getRoute();
            RequestWrapper request = this.mainRequest.getRequest();

            int maxRedirects = this.params.getIntParameter(ClientPNames.MAX_REDIRECTS, 100);
            if (this.redirectCount >= maxRedirects) {
                throw new RedirectException("Maximum redirects ("
                        + maxRedirects + ") exceeded");
            }
            this.redirectCount++;

            HttpUriRequest redirect = this.redirectStrategy.getRedirect(
                    this.currentRequest, this.currentResponse, this.localContext);
            HttpRequest orig = request.getOriginal();
            redirect.setHeaders(orig.getAllHeaders());

            URI uri = redirect.getURI();
            if (uri.getHost() == null) {
                throw new ProtocolException("Redirect URI does not specify a valid host name: " + uri);
            }
            HttpHost newTarget = new HttpHost(uri.getHost(), uri.getPort(), uri.getScheme());

            // Reset auth states if redirecting to another host
            if (!route.getTargetHost().equals(newTarget)) {
                this.log.debug("Resetting target auth state");
                this.targetAuthState.reset();
                AuthScheme authScheme = this.proxyAuthState.getAuthScheme();
                if (authScheme != null && authScheme.isConnectionBased()) {
                    this.log.debug("Resetting proxy auth state");
                    this.proxyAuthState.reset();
                }
            }

            RequestWrapper newRequest = wrapRequest(redirect);
            newRequest.setParams(this.params);

            HttpRoute newRoute = determineRoute(newTarget, newRequest, this.localContext);

            if (this.log.isDebugEnabled()) {
                this.log.debug("Redirecting to '" + uri + "' via " + newRoute);
            }
            return new RoutedRequest(newRequest, newRoute);
        }
        return null;
    }

    private RoutedRequest handleTargetChallenge(
            final CredentialsProvider credsProvider) throws HttpException {
        HttpRoute route = this.mainRequest.getRoute();
        HttpHost target = (HttpHost) this.localContext.getAttribute(
                ExecutionContext.HTTP_TARGET_HOST);
        if (target == null) {
            target = route.getTargetHost();
        }
        if (this.authenticator.isAuthenticationRequested(target, this.currentResponse,
                this.targetAuthStrategy, this.targetAuthState, this.localContext)) {
            if (this.authenticator.authenticate(target, this.currentResponse,
                    this.targetAuthStrategy, this.targetAuthState, this.localContext)) {
                // Re-try the same request via the same route
                return this.mainRequest;
            } else {
                return null;
            }
        }
        return null;
    }

    private RoutedRequest handleProxyChallenge(
            final CredentialsProvider credsProvider) throws HttpException {
        HttpRoute route = this.mainRequest.getRoute();
        HttpHost proxy = route.getProxyHost();
        if (this.authenticator.isAuthenticationRequested(proxy, this.currentResponse,
                this.proxyAuthStrategy, this.proxyAuthState, this.localContext)) {
            if (this.authenticator.authenticate(proxy, this.currentResponse,
                    this.proxyAuthStrategy, this.proxyAuthState, this.localContext)) {
                // Re-try the same request via the same route
                return this.mainRequest;
            } else {
                return null;
            }
        }
        return null;
    }

    public HttpContext getContext() {
        return this.localContext;
    }

    public HttpProcessor getHttpProcessor() {
        return this.httppocessor;
    }

    public ConnectionReuseStrategy getConnectionReuseStrategy() {
        return this.reuseStrategy;
    }

}
