/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestProxy.java,v 1.9 2004/11/20 19:35:30 olegk Exp $
 * $Revision: 1.9 $
 * $Date: 2004-11-20 20:35:30 +0100 (Sat, 20 Nov 2004) $
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
 */
package org.apache.commons.httpclient;

import org.apache.commons.httpclient.auth.AuthScheme;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.auth.CredentialsNotAvailableException;
import org.apache.commons.httpclient.auth.CredentialsProvider;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;
import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.httpclient.server.AuthRequestHandler;
import org.apache.commons.httpclient.server.HttpRequestHandlerChain;
import org.apache.commons.httpclient.server.HttpServiceHandler;
import org.apache.commons.httpclient.server.SimpleHttpServer;
import org.apache.commons.httpclient.server.SimpleProxy;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Tests for proxied connections.
 * 
 * @author Ortwin Glueck
 * @author Oleg Kalnichevski
 */
public class TestProxy extends TestCase {

    private SimpleProxy proxy = null;
    private SimpleHttpServer httpserver = null;
    private HttpClient httpclient = null;

    public TestProxy(String testName) {
        super(testName);
    }

    public static Test suite() {
        return new TestSuite(TestProxy.class);
    }

    protected void setUp() throws Exception {
        super.setUp();
        this.proxy = new SimpleProxy();
        this.httpserver = new SimpleHttpServer();
        this.httpclient = new HttpClient();
        this.httpclient.getHostConfiguration().setHost(
                this.httpserver.getLocalAddress(), 
                this.httpserver.getLocalPort(),
                Protocol.getProtocol("http"));
        this.httpclient.getHostConfiguration().setProxy(
                this.proxy.getLocalAddress(), 
                this.proxy.getLocalPort());                
    }

    protected void tearDown() throws Exception {
        this.httpclient = null;
        this.proxy.destroy();
        this.proxy = null;
        this.httpserver.destroy();
        this.httpserver = null;
        super.tearDown();
    }

    class GetItWrongThenGetItRight implements CredentialsProvider {
        
        private int hostcount = 0;
        private int proxycount = 0;
        
        public GetItWrongThenGetItRight() {
            super();
        }
        
        public Credentials getCredentials(AuthScheme scheme, String host, int port, boolean proxy)
                throws CredentialsNotAvailableException {
            if (!proxy) {
                this.hostcount++;
                return provideCredentials(this.hostcount);
            } else {
                this.proxycount++;
                return provideCredentials(this.proxycount);
            }
        }
        
        private Credentials provideCredentials(int count) {
            switch (count) {
            case 1: 
                return new UsernamePasswordCredentials("testuser", "wrongstuff");
            case 2: 
                return new UsernamePasswordCredentials("testuser", "testpass");
            default:
                return null;
            }
        }

    }
    
    /**
     * Tests GET via non-authenticating proxy
     */
    public void testSimpleGet() throws Exception {
        this.httpserver.setHttpService(new FeedbackService());
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_OK, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }
    
    /**
     * Tests GET via non-authenticating proxy + host auth + connection keep-alive 
     */
    public void testGetHostAuthConnKeepAlive() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", true));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_OK, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }
    
    /**
     * Tests GET via non-authenticating proxy + host auth + connection close 
     */
    public void testGetHostAuthConnClose() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", false));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_OK, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }

    /**
     * Tests GET via non-authenticating proxy + invalid host auth 
     */
    public void testGetHostInvalidAuth() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpclient.getState().setCredentials(AuthScope.ANY,
                new UsernamePasswordCredentials("testuser", "wrongstuff"));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_UNAUTHORIZED, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }

    /**
     * Tests GET via non-authenticating proxy + intercative host auth + connection keep-alive 
     */
    public void testGetInteractiveHostAuthConnKeepAlive() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getParams().setParameter(CredentialsProvider.PROVIDER, 
                new GetItWrongThenGetItRight());
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", true));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_OK, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }
   
    /**
     * Tests GET via non-authenticating proxy + intercative host auth + connection close 
     */
    public void testGetInteractiveHostAuthConnClose() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getParams().setParameter(CredentialsProvider.PROVIDER, 
                new GetItWrongThenGetItRight());
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", false));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_OK, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }

    /**
     * Tests POST via non-authenticating proxy
     */
    public void testSimplePost() throws Exception {
        this.httpserver.setHttpService(new FeedbackService());
        PostMethod post = new PostMethod("/");
        post.setRequestEntity(new StringRequestEntity("Like tons of stuff"));
        try {
            this.httpclient.executeMethod(post);
            assertEquals(HttpStatus.SC_OK, post.getStatusCode());
            assertNotNull(post.getResponseBodyAsString());
        } finally {
            post.releaseConnection();
        }
    }

    /**
     * Tests POST via non-authenticating proxy + host auth + connection keep-alive 
     */
    public void testPostHostAuthConnKeepAlive() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", true));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        PostMethod post = new PostMethod("/");
        post.setRequestEntity(new StringRequestEntity("Like tons of stuff"));
        try {
            this.httpclient.executeMethod(post);
            assertEquals(HttpStatus.SC_OK, post.getStatusCode());
            assertNotNull(post.getResponseBodyAsString());
        } finally {
            post.releaseConnection();
        }
    }

    /**
     * Tests POST via non-authenticating proxy + host auth + connection close 
     */
    public void testPostHostAuthConnClose() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", false));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        PostMethod post = new PostMethod("/");
        post.setRequestEntity(new StringRequestEntity("Like tons of stuff"));
        try {
            this.httpclient.executeMethod(post);
            assertEquals(HttpStatus.SC_OK, post.getStatusCode());
            assertNotNull(post.getResponseBodyAsString());
        } finally {
            post.releaseConnection();
        }
    }

    /**
     * Tests POST via non-authenticating proxy + invalid host auth 
     */
    public void testPostHostInvalidAuth() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpclient.getState().setCredentials(AuthScope.ANY,
                new UsernamePasswordCredentials("testuser", "wrongstuff"));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        PostMethod post = new PostMethod("/");
        post.setRequestEntity(new StringRequestEntity("Like tons of stuff"));
        try {
            this.httpclient.executeMethod(post);
            assertEquals(HttpStatus.SC_UNAUTHORIZED, post.getStatusCode());
        } finally {
            post.releaseConnection();
        }
    }

    /**
     * Tests POST via non-authenticating proxy + interactive host auth + connection keep-alive 
     */
    public void testPostInteractiveHostAuthConnKeepAlive() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getParams().setParameter(CredentialsProvider.PROVIDER, 
                new GetItWrongThenGetItRight());
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", true));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        PostMethod post = new PostMethod("/");
        post.setRequestEntity(new StringRequestEntity("Like tons of stuff"));
        try {
            this.httpclient.executeMethod(post);
            assertEquals(HttpStatus.SC_OK, post.getStatusCode());
            assertNotNull(post.getResponseBodyAsString());
        } finally {
            post.releaseConnection();
        }
    }

    /**
     * Tests POST via non-authenticating proxy + interactive host auth + connection close 
     */
    public void testPostInteractiveHostAuthConnClose() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getParams().setParameter(CredentialsProvider.PROVIDER, 
                new GetItWrongThenGetItRight());
                
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", false));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        PostMethod post = new PostMethod("/");
        post.setRequestEntity(new StringRequestEntity("Like tons of stuff"));
        try {
            this.httpclient.executeMethod(post);
            assertEquals(HttpStatus.SC_OK, post.getStatusCode());
            assertNotNull(post.getResponseBodyAsString());
        } finally {
            post.releaseConnection();
        }
    }
}
