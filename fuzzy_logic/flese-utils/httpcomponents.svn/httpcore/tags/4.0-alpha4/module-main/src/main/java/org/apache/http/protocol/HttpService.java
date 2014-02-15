/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-main/src/main/java/org/apache/http/protocol/HttpService.java $
 * $Revision: 510645 $
 * $Date: 2007-02-22 21:30:13 +0100 (Thu, 22 Feb 2007) $
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

package org.apache.http.protocol;

import java.io.IOException;

import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpEntity;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpResponseFactory;
import org.apache.http.HttpServerConnection;
import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.MethodNotSupportedException;
import org.apache.http.ProtocolException;
import org.apache.http.UnsupportedHttpVersionException;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.params.HttpParams;
import org.apache.http.util.EncodingUtils;

/**
 * Minimalistic server-side implementation of an HTTP processor.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 510645 $
 */
public class HttpService {

    private HttpParams params = null;
    private HttpProcessor processor = null;
    private HttpRequestHandlerResolver handlerResolver = null;
    private ConnectionReuseStrategy connStrategy = null;
    private HttpResponseFactory responseFactory = null;
    private HttpExpectationVerifier expectationVerifier = null;
    
    /**
     * Create a new HTTP service.
     *
     * @param proc             the processor to use on requests and responses
     * @param connStrategy     the connection reuse strategy
     * @param responseFactory  the response factory
     */
    public HttpService(
            final HttpProcessor proc,
            final ConnectionReuseStrategy connStrategy,
            final HttpResponseFactory responseFactory) {
        super();
        setHttpProcessor(proc);
        setConnReuseStrategy(connStrategy);
        setResponseFactory(responseFactory);
    }
    
    public void setHttpProcessor(final HttpProcessor processor) {
        if (processor == null) {
            throw new IllegalArgumentException("HTTP processor may not be null.");
        }
        this.processor = processor;
    }

    public void setConnReuseStrategy(final ConnectionReuseStrategy connStrategy) {
        if (connStrategy == null) {
            throw new IllegalArgumentException("Connection reuse strategy may not be null");
        }
        this.connStrategy = connStrategy;
    }

    public void setResponseFactory(final HttpResponseFactory responseFactory) {
        if (responseFactory == null) {
            throw new IllegalArgumentException("Response factory may not be null");
        }
        this.responseFactory = responseFactory;
    }
    
    public void setHandlerResolver(final HttpRequestHandlerResolver handlerResolver) {
        this.handlerResolver = handlerResolver;
    }

    public void setExpectationVerifier(final HttpExpectationVerifier expectationVerifier) {
        this.expectationVerifier = expectationVerifier;
    }

    public HttpParams getParams() {
        return this.params;
    }
    
    public void setParams(final HttpParams params) {
        this.params = params;
    }
    
    public void handleRequest(
            final HttpServerConnection conn, 
            final HttpContext context) throws IOException, HttpException { 
        
        context.setAttribute(HttpExecutionContext.HTTP_CONNECTION, conn);

        HttpResponse response = null;
        
        try {

            HttpRequest request = conn.receiveRequestHeader(this.params);
            HttpVersion ver = request.getRequestLine().getHttpVersion();
            if (!ver.lessEquals(HttpVersion.HTTP_1_1)) {
                // Downgrade protocol version if greater than HTTP/1.1 
                ver = HttpVersion.HTTP_1_1;
            }

            boolean receiveEntity = false;
            boolean runService = true;

            if (request instanceof HttpEntityEnclosingRequest) {
                receiveEntity = true;
                if (((HttpEntityEnclosingRequest) request).expectContinue()) {
                    response = this.responseFactory.newHttpResponse(ver, 
                            HttpStatus.SC_CONTINUE, context);
                    response.getParams().setDefaults(this.params);
                    
                    if (this.expectationVerifier != null) {
                        try {
                            this.expectationVerifier.verify(request, response, context);
                        } catch (HttpException ex) {
                            response = this.responseFactory.newHttpResponse(HttpVersion.HTTP_1_0, 
                                    HttpStatus.SC_INTERNAL_SERVER_ERROR, context);
                            response.getParams().setDefaults(this.params);
                            handleException(ex, response);
                        }
                    }
                    if (response.getStatusLine().getStatusCode() < 200) {
                        // Send 1xx response indicating the server expections
                        // have been met
                        conn.sendResponseHeader(response);
                        conn.flush();
                        response = null;
                    } else {
                        // The request does not meet the server expections
                        runService = false;
                        receiveEntity = false;
                    }
                }
            }

            if (receiveEntity) {
                conn.receiveRequestEntity((HttpEntityEnclosingRequest) request);
            }

            if (response == null) {
                response = this.responseFactory.newHttpResponse(ver, HttpStatus.SC_OK, context);
                response.getParams().setDefaults(this.params);
            }
            
            if (runService) {
                context.setAttribute(HttpExecutionContext.HTTP_REQUEST, request);
                context.setAttribute(HttpExecutionContext.HTTP_RESPONSE, response);

                this.processor.process(request, context);
                doService(request, response, context);
            }

            if (receiveEntity) {
                // Make sure the request content is fully consumed
                HttpEntity entity = ((HttpEntityEnclosingRequest)request).getEntity();
                if (entity != null) {
                    entity.consumeContent();
                }
            }
            
        } catch (HttpException ex) {
            response = this.responseFactory.newHttpResponse
                (HttpVersion.HTTP_1_0, HttpStatus.SC_INTERNAL_SERVER_ERROR,
                 context);
            response.getParams().setDefaults(this.params);
            handleException(ex, response);
        }
        
        this.processor.process(response, context);
        conn.sendResponseHeader(response);
        conn.sendResponseEntity(response);
        conn.flush();
        
        if (!this.connStrategy.keepAlive(response, context)) {
            conn.close();
        }
    }
    
    protected void handleException(final HttpException ex, final HttpResponse response) {
        if (ex instanceof MethodNotSupportedException) {
            response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            response.setStatusCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof ProtocolException) {
            response.setStatusCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            response.setStatusCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        byte[] msg = EncodingUtils.getAsciiBytes(ex.getMessage());
        ByteArrayEntity entity = new ByteArrayEntity(msg);
        entity.setContentType("text/plain; charset=US-ASCII");
        response.setEntity(entity);
    }
    
    protected void doService(
            final HttpRequest request, 
            final HttpResponse response,
            final HttpContext context) throws HttpException, IOException {
        HttpRequestHandler handler = null;
        if (this.handlerResolver != null) {
            String requestURI = request.getRequestLine().getUri();
            handler = this.handlerResolver.lookup(requestURI);
        }
        if (handler != null) {
            handler.handle(request, response, context);
        } else {
            response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
        }
    }
    
}
