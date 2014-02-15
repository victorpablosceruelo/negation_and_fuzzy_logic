/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestProxy.java,v 1.11 2004/12/11 22:35:26 olegk Exp $
 * $Revision: 1.11 $
 * $Date: 2004-12-11 23:38:35 +0100 (Sat, 11 Dec 2004) $
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

import java.util.Enumeration;

import org.apache.commons.httpclient.auth.AuthScheme;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.auth.CredentialsNotAvailableException;
import org.apache.commons.httpclient.auth.CredentialsProvider;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;
import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.httpclient.protocol.ProtocolSocketFactory;
import org.apache.commons.httpclient.server.AuthRequestHandler;
import org.apache.commons.httpclient.server.HttpRequestHandlerChain;
import org.apache.commons.httpclient.server.HttpServiceHandler;
import org.apache.commons.httpclient.server.SimpleHttpServer;
import org.apache.commons.httpclient.server.SimplePlainSocketFactory;
import org.apache.commons.httpclient.server.SimpleProxy;
import org.apache.commons.httpclient.server.SimpleSocketFactory;
import org.apache.commons.httpclient.ssl.SimpleSSLSocketFactory;
import org.apache.commons.httpclient.ssl.SimpleSSLTestProtocolSocketFactory;

import junit.extensions.TestSetup;
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
    private boolean usessl = false;

    public TestProxy(String testName) {
        super(testName);
    }

    static class SSLDecorator extends TestSetup {

        public static void addTests(TestSuite suite) {
            TestSuite ts2 = new TestSuite();
            addTest(ts2, suite);
            suite.addTest(ts2);        
        }
        
        private static void addTest(TestSuite suite, Test t) {
            if (t instanceof TestProxy) {
                suite.addTest(new SSLDecorator((TestProxy) t));
            } else if (t instanceof TestSuite) {
                Enumeration en = ((TestSuite) t).tests();
                while (en.hasMoreElements()) {
                    addTest(suite, (Test) en.nextElement());
                }
            }
        }
        
        public SSLDecorator(TestProxy test) {
            super(test);
        }
                
        protected void setUp() throws Exception {
            TestProxy base = (TestProxy)getTest();
            base.setUseSSL(true);
        }  
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(TestProxy.class);
        SSLDecorator.addTests(suite);
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
        this.httpclient = new HttpClient();
        this.proxy = new SimpleProxy();
        this.httpclient.getHostConfiguration().setProxy(
                this.proxy.getLocalAddress(), 
                this.proxy.getLocalPort());

        SimpleSocketFactory serversocketfactory = null; 
        Protocol testhttp = null;
        if (this.usessl) {
            serversocketfactory = new SimpleSSLSocketFactory(); 
            testhttp = new Protocol("https", 
                    (ProtocolSocketFactory)new SimpleSSLTestProtocolSocketFactory(), 443);
        } else {
            serversocketfactory = new SimplePlainSocketFactory(); 
            testhttp = Protocol.getProtocol("http"); 
        }
        this.httpserver = new SimpleHttpServer(serversocketfactory, 0);
        this.httpclient.getHostConfiguration().setHost(
                this.httpserver.getLocalAddress(), 
                this.httpserver.getLocalPort(),
                testhttp);
    }

    protected void tearDown() throws Exception {
        this.httpclient = null;
        this.proxy.destroy();
        this.proxy = null;
        this.httpserver.destroy();
        this.httpserver = null;
        super.tearDown();
    }

    public void setUseSSL(boolean b) {
        this.usessl = b;
    }
    
    public boolean isUseSSL() {
        return this.usessl;
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
     * Tests GET via non-authenticating proxy + interactive host auth + connection keep-alive 
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
     * Tests GET via non-authenticating proxy + interactive host auth + connection close 
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
     * Tests GET via authenticating proxy + host auth + connection keep-alive 
     */
    public void testGetProxyAuthHostAuthConnKeepAlive() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        this.httpclient.getState().setProxyCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", true));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        this.proxy.requireAuthentication(creds, "test", true);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_OK, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }
    
    /**
     * Tests GET via authenticating proxy
     */
    public void testGetAuthProxy() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setProxyCredentials(AuthScope.ANY, creds);
        this.httpserver.setHttpService(new FeedbackService());

        this.proxy.requireAuthentication(creds, "test", true);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_OK, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }
    
    /**
     * Tests GET via authenticating proxy + host auth + connection close 
     */
    public void testGetProxyAuthHostAuthConnClose() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        this.httpclient.getState().setProxyCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", false));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);

        this.proxy.requireAuthentication(creds, "test", true);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_OK, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }
    
    /**
     * Tests GET via authenticating proxy + invalid host auth 
     */
    public void testGetProxyAuthHostInvalidAuth() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpclient.getState().setCredentials(AuthScope.ANY,
                new UsernamePasswordCredentials("testuser", "wrongstuff"));
        this.httpclient.getState().setProxyCredentials(AuthScope.ANY, creds);
        
        this.httpserver.setRequestHandler(handlerchain);
        
        this.proxy.requireAuthentication(creds, "test", true);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_UNAUTHORIZED, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }

    /**
     * Tests GET via authenticating proxy + interactive host and proxy auth + connection keep-alive 
     */
    public void testGetInteractiveProxyAuthHostAuthConnKeepAlive() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getParams().setParameter(CredentialsProvider.PROVIDER, 
                new GetItWrongThenGetItRight());
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", true));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);

        this.proxy.requireAuthentication(creds, "test", true);
        
        GetMethod get = new GetMethod("/");
        try {
            this.httpclient.executeMethod(get);
            assertEquals(HttpStatus.SC_OK, get.getStatusCode());
        } finally {
            get.releaseConnection();
        }
    }

    /**
     * Tests GET via authenticating proxy + interactive host and proxy auth + connection close 
     */
    public void testGetInteractiveProxyAuthHostAuthConnClose() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getParams().setParameter(CredentialsProvider.PROVIDER, 
                new GetItWrongThenGetItRight());
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", false));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        this.proxy.requireAuthentication(creds, "test", true);
        
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

    /**
     * Tests POST via authenticating proxy
     */
    public void testPostAuthProxy() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setProxyCredentials(AuthScope.ANY, creds);
        this.httpserver.setHttpService(new FeedbackService());

        this.proxy.requireAuthentication(creds, "test", true);

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
     * Tests POST via authenticating proxy + host auth + connection keep-alive 
     */
    public void testPostProxyAuthHostAuthConnKeepAlive() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        this.httpclient.getState().setProxyCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", true));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        this.proxy.requireAuthentication(creds, "test", true);

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
     * Tests POST via authenticating proxy + host auth + connection close 
     */
    public void testPostProxyAuthHostAuthConnClose() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setCredentials(AuthScope.ANY, creds);
        this.httpclient.getState().setProxyCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", false));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        this.proxy.requireAuthentication(creds, "test", true);

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
    public void testPostProxyAuthHostInvalidAuth() throws Exception {

        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getState().setProxyCredentials(AuthScope.ANY, creds);
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpclient.getState().setCredentials(AuthScope.ANY,
                new UsernamePasswordCredentials("testuser", "wrongstuff"));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        this.proxy.requireAuthentication(creds, "test", true);

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
    public void testPostInteractiveProxyAuthHostAuthConnKeepAlive() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getParams().setParameter(CredentialsProvider.PROVIDER, 
                new GetItWrongThenGetItRight());
        
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", true));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        this.proxy.requireAuthentication(creds, "test", true);

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
    public void testPostInteractiveProxyAuthHostAuthConnClose() throws Exception {
        UsernamePasswordCredentials creds = 
            new UsernamePasswordCredentials("testuser", "testpass");
        
        this.httpclient.getParams().setParameter(CredentialsProvider.PROVIDER, 
                new GetItWrongThenGetItRight());
                
        HttpRequestHandlerChain handlerchain = new HttpRequestHandlerChain();
        handlerchain.appendHandler(new AuthRequestHandler(creds, "test", false));
        handlerchain.appendHandler(new HttpServiceHandler(new FeedbackService()));
        
        this.httpserver.setRequestHandler(handlerchain);
        
        this.proxy.requireAuthentication(creds, "test", true);

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
