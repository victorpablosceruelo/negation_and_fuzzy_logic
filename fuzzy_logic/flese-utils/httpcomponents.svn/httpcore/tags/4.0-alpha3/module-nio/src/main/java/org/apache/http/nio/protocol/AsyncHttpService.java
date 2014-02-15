package org.apache.http.nio.protocol;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpEntity;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpResponseFactory;
import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.MethodNotSupportedException;
import org.apache.http.ProtocolException;
import org.apache.http.UnsupportedHttpVersionException;
import org.apache.http.entity.BasicHttpEntity;
import org.apache.http.entity.StringEntity;
import org.apache.http.nio.ContentDecoder;
import org.apache.http.nio.ContentEncoder;
import org.apache.http.nio.NHttpServerConnection;
import org.apache.http.nio.util.ContentInputBuffer;
import org.apache.http.nio.util.ContentOutputBuffer;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpExecutionContext;
import org.apache.http.protocol.HttpProcessor;
import org.apache.http.protocol.HttpRequestHandler;
import org.apache.http.protocol.HttpRequestHandlerResolver;

public class AsyncHttpService {

    private final ContentInputBuffer inbuffer; 
    private final ContentOutputBuffer outbuffer; 
    
    private HttpParams params = null;
    private HttpResponseFactory responseFactory = null;
    private HttpProcessor httpProcessor = null;
    private HttpRequestHandlerResolver handlerResolver = null;
    private ConnectionReuseStrategy connStrategy = null;
    
    public AsyncHttpService(
            final ContentInputBuffer inbuffer,
            final ContentOutputBuffer outbuffer,
            final HttpProcessor proc,
            final ConnectionReuseStrategy connStrategy,
            final HttpResponseFactory responseFactory) {
        super();
        if (inbuffer == null) {
            throw new IllegalArgumentException("Content input buffer may not be null");
        }
        if (outbuffer == null) {
            throw new IllegalArgumentException("Content output buffer may not be null");
        }
        this.inbuffer = inbuffer; 
        this.outbuffer = outbuffer; 
        setHttpProcessor(proc);
        setConnReuseStrategy(connStrategy);
        setResponseFactory(responseFactory);
    }

    public void setHttpProcessor(final HttpProcessor processor) {
        if (processor == null) {
            throw new IllegalArgumentException("HTTP processor may not be null.");
        }
        this.httpProcessor = processor;
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

    public HttpParams getParams() {
        return this.params;
    }
    
    public void setParams(final HttpParams params) {
        this.params = params;
    }
    
    public void shutdown() {
        this.inbuffer.shutdown();
        this.outbuffer.shutdown();
    }
    
    public void shutdown(final IOException ex) {
        this.inbuffer.shutdown(ex);
        this.outbuffer.shutdown(ex);
    }
    
    public void handleRequest(final HttpRequest request, final NHttpServerConnection conn) 
                throws HttpException, IOException {
        HttpContext parentContext = conn.getContext();
        HttpVersion ver = request.getRequestLine().getHttpVersion();
        if (!ver.lessEquals(HttpVersion.HTTP_1_1)) {
            // Downgrade protocol version if greater than HTTP/1.1 
            ver = HttpVersion.HTTP_1_1;
        }
        if (request instanceof HttpEntityEnclosingRequest) {
            HttpEntityEnclosingRequest entityReq = (HttpEntityEnclosingRequest) request;
            if (entityReq.expectContinue()) {
                HttpResponse ack = this.responseFactory.newHttpResponse(ver, 100);
                conn.submitResponse(ack);
            }
            // Re-create the enclosed entity
            HttpEntity entity = entityReq.getEntity();
            BasicHttpEntity newEntity = new BasicHttpEntity();
            InputStream instream = new ContentInputStream(this.inbuffer);
            newEntity.setContent(instream);
            newEntity.setChunked(entity.isChunked());
            newEntity.setContentLength(entity.getContentLength());
            newEntity.setContentType(entity.getContentType());
            newEntity.setContentEncoding(entity.getContentEncoding());
            
            entityReq.setEntity(newEntity);
        }
        
        // Generate response
        request.getParams().setDefaults(this.params);
        HttpResponse response = this.responseFactory.newHttpResponse(ver, HttpStatus.SC_OK);
        response.getParams().setDefaults(this.params);
        
        // Configure HTTP context
        HttpContext context = new HttpExecutionContext(parentContext);
        context.setAttribute(HttpExecutionContext.HTTP_CONNECTION, conn);
        context.setAttribute(HttpExecutionContext.HTTP_REQUEST, request);
        context.setAttribute(HttpExecutionContext.HTTP_RESPONSE, response);
        
        try {
            // Pre-process the request
            this.httpProcessor.process(request, context);
            
            // Call HTTP request handler
            doService(request, response, context);

            // Post-process the response
            this.httpProcessor.process(response, context);
        } catch (HttpException ex) {
            handleException(conn, ex);
            return;
        }

        // Make sure the request entity is fully consumed
        if (request instanceof HttpEntityEnclosingRequest) {
            HttpEntityEnclosingRequest entityReq = (HttpEntityEnclosingRequest) request;
            HttpEntity entity = entityReq.getEntity();
            entity.consumeContent();
        }
        
        // Commit the response
        conn.submitResponse(response);

        HttpEntity entity = response.getEntity();
        if (entity != null) {
            ContentOutputStream outstream = new ContentOutputStream(this.outbuffer);
            entity.writeTo(outstream);
            outstream.flush();
        }

        // Test if the connection can be reused
        if (this.connStrategy.keepAlive(response, context)) {
            this.inbuffer.reset();
            this.outbuffer.reset();
        } else {
            conn.close();
        }
    }
    
    public void handleException(final NHttpServerConnection conn, final HttpException ex)
                throws HttpException, IOException {
        // Generate response
        HttpContext context = conn.getContext();
        HttpResponse response = this.responseFactory.newHttpResponse(
                HttpVersion.HTTP_1_0, HttpStatus.SC_OK);
        response.getParams().setDefaults(this.params);
        
        if (ex instanceof MethodNotSupportedException) {
            response.setStatusCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            response.setStatusCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof ProtocolException) {
            response.setStatusCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            response.setStatusCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        StringEntity entity = new StringEntity(ex.getMessage(), "US-ASCII");
        entity.setContentEncoding("text/plain; charset=US-ASCII");
        response.setEntity(entity);
        
        // Post-process the response
        this.httpProcessor.process(response, context);

        // Commit the response
        conn.submitResponse(response);
        ContentOutputStream outstream = new ContentOutputStream(this.outbuffer);
        entity.writeTo(outstream);
        outstream.flush();

        // Test if the connection can be reused
        if (this.connStrategy.keepAlive(response, context)) {
            this.inbuffer.reset();
            this.outbuffer.reset();
        } else {
            conn.close();
        }
    }
    
    public void consumeContent(final ContentDecoder decoder) throws IOException {
        this.inbuffer.consumeContent(decoder);
    }

    public void produceContent(final ContentEncoder encoder) throws IOException {
        this.outbuffer.produceContent(encoder);
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
    
    private static class ContentInputStream extends InputStream {

        private final ContentInputBuffer buffer;
        
        public ContentInputStream(final ContentInputBuffer buffer) {
            super();
            if (buffer == null) {
                throw new IllegalArgumentException("Input buffer may not be null");
            }
            this.buffer = buffer;
        }
        
        public int read(final byte[] b, int off, int len) throws IOException {
            return this.buffer.read(b, off, len);
        }
        
        public int read(final byte[] b) throws IOException {
            return this.buffer.read(b);
        }
        
        public int read() throws IOException {
            return this.buffer.read();
        }

    }    

    private static class ContentOutputStream extends OutputStream {

        private final ContentOutputBuffer buffer;
        
        public ContentOutputStream(final ContentOutputBuffer buffer) {
            super();
            if (buffer == null) {
                throw new IllegalArgumentException("Output buffer may not be null");
            }
            this.buffer = buffer;
        }

        public void close() throws IOException {
            this.buffer.flush();
            this.buffer.shutdown();
        }

        public void flush() throws IOException {
            this.buffer.flush();
        }

        public void write(byte[] b, int off, int len) throws IOException {
            this.buffer.write(b, off, len);
        }

        public void write(byte[] b) throws IOException {
            this.buffer.write(b);
        }

        public void write(int b) throws IOException {
            this.buffer.write(b);
        }

    }

}
