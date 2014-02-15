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

package org.apache.http.conn.ssl;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URL;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;

import org.apache.http.HttpHost;
import org.apache.http.localserver.LocalServerTestBase;
import org.apache.http.localserver.LocalTestServer;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for {@link SSLSocketFactory}.
 */
public class TestSSLSocketFactory extends LocalServerTestBase {

    private KeyManagerFactory createKeyManagerFactory() throws NoSuchAlgorithmException {
        final String algo = KeyManagerFactory.getDefaultAlgorithm();
        try {
            return KeyManagerFactory.getInstance(algo);
        } catch (final NoSuchAlgorithmException ex) {
            return KeyManagerFactory.getInstance("SunX509");
        }
    }

    private TrustManagerFactory createTrustManagerFactory() throws NoSuchAlgorithmException {
        final String algo = TrustManagerFactory.getDefaultAlgorithm();
        try {
            return TrustManagerFactory.getInstance(algo);
        } catch (final NoSuchAlgorithmException ex) {
            return TrustManagerFactory.getInstance("SunX509");
        }
    }

    private SSLContext serverSSLContext;
    private SSLContext clientSSLContext;

    @Before
    public void setUp() throws Exception {
        final ClassLoader cl = getClass().getClassLoader();
        final URL url = cl.getResource("test.keystore");
        final KeyStore keystore  = KeyStore.getInstance("jks");
        final char[] pwd = "nopassword".toCharArray();
        keystore.load(url.openStream(), pwd);

        final TrustManagerFactory tmf = createTrustManagerFactory();
        tmf.init(keystore);
        final TrustManager[] tm = tmf.getTrustManagers();

        final KeyManagerFactory kmfactory = createKeyManagerFactory();
        kmfactory.init(keystore, pwd);
        final KeyManager[] km = kmfactory.getKeyManagers();

        this.serverSSLContext = SSLContext.getInstance("TLS");
        this.serverSSLContext.init(km, tm, null);

        this.clientSSLContext = SSLContext.getInstance("TLS");
        this.clientSSLContext.init(null, tm, null);

        this.localServer = new LocalTestServer(this.serverSSLContext);
        this.localServer.registerDefaultHandlers();

        this.localServer.start();
    }

    @Override
    protected HttpHost getServerHttp() {
        final InetSocketAddress address = this.localServer.getServiceAddress();
        return new HttpHost(
                address.getHostName(),
                address.getPort(),
                "https");
    }

    static class TestX509HostnameVerifier implements X509HostnameVerifier {

        private boolean fired = false;

        public boolean verify(final String host, final SSLSession session) {
            return true;
        }

        public void verify(final String host, final SSLSocket ssl) throws IOException {
            this.fired = true;
        }

        public void verify(final String host, final String[] cns, final String[] subjectAlts) throws SSLException {
        }

        public void verify(final String host, final X509Certificate cert) throws SSLException {
        }

        public boolean isFired() {
            return this.fired;
        }

    }

    @Test
    public void testBasicSSL() throws Exception {
        final HttpHost host = new HttpHost("localhost", 443, "https");
        final HttpContext context = new BasicHttpContext();
        final TestX509HostnameVerifier hostVerifier = new TestX509HostnameVerifier();
        final SSLSocketFactory socketFactory = new SSLSocketFactory(this.clientSSLContext, hostVerifier);
        SSLSocket socket = (SSLSocket) socketFactory.createSocket(context);
        final InetSocketAddress remoteAddress = this.localServer.getServiceAddress();
        socket = (SSLSocket) socketFactory.connectSocket(0, socket, host, remoteAddress, null, context);
        final SSLSession sslsession = socket.getSession();

        Assert.assertNotNull(sslsession);
        Assert.assertTrue(hostVerifier.isFired());
    }

    @Test(expected=SSLHandshakeException.class)
    public void testSSLTrustVerification() throws Exception {
        final HttpHost host = new HttpHost("localhost", 443, "https");
        final HttpContext context = new BasicHttpContext();
        // Use default SSL context
        final SSLContext defaultsslcontext = SSLContext.getInstance("TLS");
        defaultsslcontext.init(null, null, null);

        final SSLSocketFactory socketFactory = new SSLSocketFactory(defaultsslcontext,
                SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER);

        final SSLSocket socket = (SSLSocket) socketFactory.createSocket(context);
        final InetSocketAddress remoteAddress = this.localServer.getServiceAddress();
        socketFactory.connectSocket(0, socket, host, remoteAddress, null, context);
    }

    @Test
    public void testSSLTrustVerificationOverride() throws Exception {
        final HttpHost host = new HttpHost("localhost", 443, "https");
        final HttpContext context = new BasicHttpContext();
        // Use default SSL context
        final SSLContext defaultsslcontext = SSLContext.getInstance("TLS");
        defaultsslcontext.init(null, null, null);

        final TrustStrategy trustStrategy = new TrustStrategy() {

            public boolean isTrusted(
                    final X509Certificate[] chain, final String authType) throws CertificateException {
                return chain.length == 1;
            }

        };
        final SSLContext sslcontext = SSLContexts.custom()
            .loadTrustMaterial(null, null, trustStrategy)
            .build();
        final SSLSocketFactory socketFactory = new SSLSocketFactory(
                sslcontext,
                SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER);

        final SSLSocket socket = (SSLSocket) socketFactory.createSocket(context);
        final InetSocketAddress remoteAddress = this.localServer.getServiceAddress();
        socketFactory.connectSocket(0, socket, host, remoteAddress, null, context);
    }

    @Test
    public void testKeyWithAlternatePassword() throws Exception {
        final String keystorePassword = "nopassword";
        final String keyPassword = "password";

        final ClassLoader cl = getClass().getClassLoader();
        final URL url = cl.getResource("test-keypasswd.keystore");
        final KeyStore keystore  = KeyStore.getInstance("jks");
        keystore.load(url.openStream(), keystorePassword.toCharArray());

        final SSLContext sslcontext = SSLContexts.custom()
                .loadKeyMaterial(keystore, keyPassword != null ? keyPassword.toCharArray() : null)
                .loadTrustMaterial(keystore)
                .build();
        new SSLSocketFactory(sslcontext);
    }

    @Test(expected=UnrecoverableKeyException.class)
    public void testKeyWithAlternatePasswordInvalid() throws Exception {
        final String keystorePassword = "nopassword";
        final String keyPassword = "!password";

        final ClassLoader cl = getClass().getClassLoader();
        final URL url = cl.getResource("test-keypasswd.keystore");
        final KeyStore keystore  = KeyStore.getInstance("jks");
        keystore.load(url.openStream(), keystorePassword.toCharArray());

        final SSLContext sslcontext = SSLContexts.custom()
                .loadKeyMaterial(keystore, keyPassword != null ? keyPassword.toCharArray() : null)
                .loadTrustMaterial(keystore)
                .build();
        new SSLSocketFactory(sslcontext);
    }
}
