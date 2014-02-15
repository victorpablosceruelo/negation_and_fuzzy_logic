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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SecureRandom;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import org.apache.http.HttpHost;
import org.apache.http.annotation.ThreadSafe;
import org.apache.http.conn.ConnectTimeoutException;
import org.apache.http.conn.HttpInetSocketAddress;
import org.apache.http.conn.scheme.HostNameResolver;
import org.apache.http.conn.scheme.LayeredSchemeSocketFactory;
import org.apache.http.conn.scheme.LayeredSocketFactory;
import org.apache.http.conn.scheme.SchemeLayeredSocketFactory;
import org.apache.http.conn.socket.LayeredConnectionSocketFactory;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HttpContext;
import org.apache.http.util.Args;
import org.apache.http.util.Asserts;

/**
 * Layered socket factory for TLS/SSL connections.
 * <p>
 * SSLSocketFactory can be used to validate the identity of the HTTPS server against a list of
 * trusted certificates and to authenticate to the HTTPS server using a private key.
 * <p>
 * SSLSocketFactory will enable server authentication when supplied with
 * a {@link KeyStore trust-store} file containing one or several trusted certificates. The client
 * secure socket will reject the connection during the SSL session handshake if the target HTTPS
 * server attempts to authenticate itself with a non-trusted certificate.
 * <p>
 * Use JDK keytool utility to import a trusted certificate and generate a trust-store file:
 *    <pre>
 *     keytool -import -alias "my server cert" -file server.crt -keystore my.truststore
 *    </pre>
 * <p>
 * In special cases the standard trust verification process can be bypassed by using a custom
 * {@link TrustStrategy}. This interface is primarily intended for allowing self-signed
 * certificates to be accepted as trusted without having to add them to the trust-store file.
 * <p>
 * SSLSocketFactory will enable client authentication when supplied with
 * a {@link KeyStore key-store} file containing a private key/public certificate
 * pair. The client secure socket will use the private key to authenticate
 * itself to the target HTTPS server during the SSL session handshake if
 * requested to do so by the server.
 * The target HTTPS server will in its turn verify the certificate presented
 * by the client in order to establish client's authenticity
 * <p>
 * Use the following sequence of actions to generate a key-store file
 * </p>
 *   <ul>
 *     <li>
 *      <p>
 *      Use JDK keytool utility to generate a new key
 *      <pre>keytool -genkey -v -alias "my client key" -validity 365 -keystore my.keystore</pre>
 *      For simplicity use the same password for the key as that of the key-store
 *      </p>
 *     </li>
 *     <li>
 *      <p>
 *      Issue a certificate signing request (CSR)
 *      <pre>keytool -certreq -alias "my client key" -file mycertreq.csr -keystore my.keystore</pre>
 *     </p>
 *     </li>
 *     <li>
 *      <p>
 *      Send the certificate request to the trusted Certificate Authority for signature.
 *      One may choose to act as her own CA and sign the certificate request using a PKI
 *      tool, such as OpenSSL.
 *      </p>
 *     </li>
 *     <li>
 *      <p>
 *       Import the trusted CA root certificate
 *       <pre>keytool -import -alias "my trusted ca" -file caroot.crt -keystore my.keystore</pre>
 *      </p>
 *     </li>
 *     <li>
 *      <p>
 *       Import the PKCS#7 file containg the complete certificate chain
 *       <pre>keytool -import -alias "my client key" -file mycert.p7 -keystore my.keystore</pre>
 *      </p>
 *     </li>
 *     <li>
 *      <p>
 *       Verify the content the resultant keystore file
 *       <pre>keytool -list -v -keystore my.keystore</pre>
 *      </p>
 *     </li>
 *   </ul>
 *
 * @since 4.0
 */
@SuppressWarnings("deprecation")
@ThreadSafe
public class SSLSocketFactory implements LayeredConnectionSocketFactory, SchemeLayeredSocketFactory,
                                         LayeredSchemeSocketFactory, LayeredSocketFactory {

    public static final String TLS   = "TLS";
    public static final String SSL   = "SSL";
    public static final String SSLV2 = "SSLv2";

    public static final X509HostnameVerifier ALLOW_ALL_HOSTNAME_VERIFIER
        = new AllowAllHostnameVerifier();

    public static final X509HostnameVerifier BROWSER_COMPATIBLE_HOSTNAME_VERIFIER
        = new BrowserCompatHostnameVerifier();

    public static final X509HostnameVerifier STRICT_HOSTNAME_VERIFIER
        = new StrictHostnameVerifier();

    private final static char[] EMPTY_PASSWORD = "".toCharArray();

    /**
     * Gets the default factory, which uses the default JSSE settings for initializing
     * the SSL context.
     *
     * @return the default SSL socket factory
     */
    public static SSLSocketFactory getSocketFactory() throws SSLInitializationException {
        return new SSLSocketFactory(createDefaultSSLContext());
    }

    /**
     * Gets the default factory, which uses system properties for initializing the SSL context
     * as described in
     * <a href="http://docs.oracle.com/javase/1.5.0/docs/guide/security/jsse/JSSERefGuide.html">
     * "JavaTM Secure Socket Extension (JSSE) Reference Guide for the JavaTM 2 Platform
     * Standard Edition 5</a>
     * <p>
     * The following system properties are taken into account by this method:
     * <ul>
     *  <li>ssl.TrustManagerFactory.algorithm</li>
     *  <li>javax.net.ssl.trustStoreType</li>
     *  <li>javax.net.ssl.trustStore</li>
     *  <li>javax.net.ssl.trustStoreProvider</li>
     *  <li>javax.net.ssl.trustStorePassword</li>
     *  <li>java.home</li>
     *  <li>ssl.KeyManagerFactory.algorithm</li>
     *  <li>javax.net.ssl.keyStoreType</li>
     *  <li>javax.net.ssl.keyStore</li>
     *  <li>javax.net.ssl.keyStoreProvider</li>
     *  <li>javax.net.ssl.keyStorePassword</li>
     * </ul>
     * <p>
     *
     * @return the system SSL socket factory
     */
    public static SSLSocketFactory getSystemSocketFactory() throws SSLInitializationException {
        return new SSLSocketFactory(createSystemSSLContext());
    }

    private final javax.net.ssl.SSLSocketFactory socketfactory;
    private final HostNameResolver nameResolver;
    // TODO: make final
    private volatile X509HostnameVerifier hostnameVerifier;

    private static SSLContext createSSLContext(
            String algorithm,
            final KeyStore keystore,
            final String keystorePassword,
            final KeyStore truststore,
            final SecureRandom random,
            final TrustStrategy trustStrategy)
                throws NoSuchAlgorithmException, KeyStoreException, UnrecoverableKeyException, KeyManagementException {
        if (algorithm == null) {
            algorithm = TLS;
        }
        final KeyManagerFactory kmfactory = KeyManagerFactory.getInstance(
                KeyManagerFactory.getDefaultAlgorithm());
        kmfactory.init(keystore, keystorePassword != null ? keystorePassword.toCharArray(): null);
        final KeyManager[] keymanagers =  kmfactory.getKeyManagers();
        final TrustManagerFactory tmfactory = TrustManagerFactory.getInstance(
                TrustManagerFactory.getDefaultAlgorithm());
        tmfactory.init(truststore);
        final TrustManager[] trustmanagers = tmfactory.getTrustManagers();
        if (trustmanagers != null && trustStrategy != null) {
            for (int i = 0; i < trustmanagers.length; i++) {
                final TrustManager tm = trustmanagers[i];
                if (tm instanceof X509TrustManager) {
                    trustmanagers[i] = new TrustManagerDecorator(
                            (X509TrustManager) tm, trustStrategy);
                }
            }
        }

        final SSLContext sslcontext = SSLContext.getInstance(algorithm);
        sslcontext.init(keymanagers, trustmanagers, random);
        return sslcontext;
    }

    private static SSLContext createSystemSSLContext(
            String algorithm,
            final SecureRandom random) throws IOException, NoSuchAlgorithmException, NoSuchProviderException,
            KeyStoreException, CertificateException, UnrecoverableKeyException, KeyManagementException {
        if (algorithm == null) {
            algorithm = TLS;
        }
        TrustManagerFactory tmfactory = null;

        String trustAlgorithm = System.getProperty("ssl.TrustManagerFactory.algorithm");
        if (trustAlgorithm == null) {
            trustAlgorithm = TrustManagerFactory.getDefaultAlgorithm();
        }
        String trustStoreType = System.getProperty("javax.net.ssl.trustStoreType");
        if (trustStoreType == null) {
            trustStoreType = KeyStore.getDefaultType();
        }
        if ("none".equalsIgnoreCase(trustStoreType)) {
            tmfactory = TrustManagerFactory.getInstance(trustAlgorithm);
        } else {
            File trustStoreFile = null;
            final String s = System.getProperty("javax.net.ssl.trustStore");
            if (s != null) {
                trustStoreFile = new File(s);
                tmfactory = TrustManagerFactory.getInstance(trustAlgorithm);
                final String trustStoreProvider = System.getProperty("javax.net.ssl.trustStoreProvider");
                KeyStore trustStore;
                if (trustStoreProvider != null) {
                    trustStore = KeyStore.getInstance(trustStoreType, trustStoreProvider);
                } else {
                    trustStore = KeyStore.getInstance(trustStoreType);
                }
                final String trustStorePassword = System.getProperty("javax.net.ssl.trustStorePassword");
                final FileInputStream instream = new FileInputStream(trustStoreFile);
                try {
                    trustStore.load(instream, trustStorePassword != null ?
                            trustStorePassword.toCharArray() : EMPTY_PASSWORD);
                } finally {
                    instream.close();
                }
                tmfactory.init(trustStore);
            } else {
                final File javaHome = new File(System.getProperty("java.home"));
                File file = new File(javaHome, "lib/security/jssecacerts");
                if (!file.exists()) {
                    file = new File(javaHome, "lib/security/cacerts");
                    trustStoreFile = file;
                } else {
                    trustStoreFile = file;
                }
                tmfactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
                final KeyStore trustStore = KeyStore.getInstance(KeyStore.getDefaultType());
                final String trustStorePassword = System.getProperty("javax.net.ssl.trustStorePassword");
                final FileInputStream instream = new FileInputStream(trustStoreFile);
                try {
                    trustStore.load(instream, trustStorePassword != null ? trustStorePassword.toCharArray() : null);
                } finally {
                    instream.close();
                }
                tmfactory.init(trustStore);
            }
        }

        KeyManagerFactory kmfactory = null;
        String keyAlgorithm = System.getProperty("ssl.KeyManagerFactory.algorithm");
        if (keyAlgorithm == null) {
            keyAlgorithm = KeyManagerFactory.getDefaultAlgorithm();
        }
        String keyStoreType = System.getProperty("javax.net.ssl.keyStoreType");
        if (keyStoreType == null) {
            keyStoreType = KeyStore.getDefaultType();
        }
        if ("none".equalsIgnoreCase(keyStoreType)) {
            kmfactory = KeyManagerFactory.getInstance(keyAlgorithm);
        } else {
            File keyStoreFile = null;
            final String s = System.getProperty("javax.net.ssl.keyStore");
            if (s != null) {
                keyStoreFile = new File(s);
            }
            if (keyStoreFile != null) {
                kmfactory = KeyManagerFactory.getInstance(keyAlgorithm);
                final String keyStoreProvider = System.getProperty("javax.net.ssl.keyStoreProvider");
                KeyStore keyStore;
                if (keyStoreProvider != null) {
                    keyStore = KeyStore.getInstance(keyStoreType, keyStoreProvider);
                } else {
                    keyStore = KeyStore.getInstance(keyStoreType);
                }
                final String keyStorePassword = System.getProperty("javax.net.ssl.keyStorePassword");
                final FileInputStream instream = new FileInputStream(keyStoreFile);
                try {
                    keyStore.load(instream, keyStorePassword != null ?
                            keyStorePassword.toCharArray() : EMPTY_PASSWORD);
                } finally {
                    instream.close();
                }
                kmfactory.init(keyStore, keyStorePassword != null ?
                        keyStorePassword.toCharArray() : EMPTY_PASSWORD);
            }
        }

        final SSLContext sslcontext = SSLContext.getInstance(algorithm);
        sslcontext.init(
                kmfactory != null ? kmfactory.getKeyManagers() : null,
                tmfactory != null ? tmfactory.getTrustManagers() : null,
                random);
        return sslcontext;
    }

    /**
     * @since 4.3
     */
    public static SSLContext createDefaultSSLContext() throws SSLInitializationException {
        try {
            return createSSLContext(TLS, null, null, null, null, null);
        } catch (final Exception ex) {
            throw new SSLInitializationException("Failure initializing default SSL context", ex);
        }
    }

    /**
     * @since 4.3
     */
    public static SSLContext createSystemSSLContext() throws SSLInitializationException {
        try {
            return createSystemSSLContext(TLS, null);
        } catch (final Exception ex) {
            throw new SSLInitializationException("Failure initializing default system SSL context", ex);
        }
    }

    /**
     * @deprecated (4.1) Use {@link #SSLSocketFactory(String, KeyStore, String, KeyStore,
     *   SecureRandom, X509HostnameVerifier)}
     */
    @Deprecated
    public SSLSocketFactory(
            final String algorithm,
            final KeyStore keystore,
            final String keystorePassword,
            final KeyStore truststore,
            final SecureRandom random,
            final HostNameResolver nameResolver)
                throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        this(createSSLContext(
                algorithm, keystore, keystorePassword, truststore, random, null),
                nameResolver);
    }

    /**
     * @since 4.1
     */
    public SSLSocketFactory(
            final String algorithm,
            final KeyStore keystore,
            final String keystorePassword,
            final KeyStore truststore,
            final SecureRandom random,
            final X509HostnameVerifier hostnameVerifier)
                throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        this(createSSLContext(
                algorithm, keystore, keystorePassword, truststore, random, null),
                hostnameVerifier);
    }

    /**
     * @since 4.1
     */
    public SSLSocketFactory(
            final String algorithm,
            final KeyStore keystore,
            final String keystorePassword,
            final KeyStore truststore,
            final SecureRandom random,
            final TrustStrategy trustStrategy,
            final X509HostnameVerifier hostnameVerifier)
                throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        this(createSSLContext(
                algorithm, keystore, keystorePassword, truststore, random, trustStrategy),
                hostnameVerifier);
    }

    public SSLSocketFactory(
            final KeyStore keystore,
            final String keystorePassword,
            final KeyStore truststore)
                throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        this(TLS, keystore, keystorePassword, truststore, null, null, BROWSER_COMPATIBLE_HOSTNAME_VERIFIER);
    }

    public SSLSocketFactory(
            final KeyStore keystore,
            final String keystorePassword)
                throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException{
        this(TLS, keystore, keystorePassword, null, null, null, BROWSER_COMPATIBLE_HOSTNAME_VERIFIER);
    }

    public SSLSocketFactory(
            final KeyStore truststore)
                throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        this(TLS, null, null, truststore, null, null, BROWSER_COMPATIBLE_HOSTNAME_VERIFIER);
    }

    /**
     * @since 4.1
     */
    public SSLSocketFactory(
            final TrustStrategy trustStrategy,
            final X509HostnameVerifier hostnameVerifier)
                throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        this(TLS, null, null, null, null, trustStrategy, hostnameVerifier);
    }

    /**
     * @since 4.1
     */
    public SSLSocketFactory(
            final TrustStrategy trustStrategy)
                throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        this(TLS, null, null, null, null, trustStrategy, BROWSER_COMPATIBLE_HOSTNAME_VERIFIER);
    }

    public SSLSocketFactory(final SSLContext sslContext) {
        this(sslContext, BROWSER_COMPATIBLE_HOSTNAME_VERIFIER);
    }

    /**
     * @deprecated (4.1) Use {@link #SSLSocketFactory(SSLContext)}
     */
    @Deprecated
    public SSLSocketFactory(
            final SSLContext sslContext, final HostNameResolver nameResolver) {
        super();
        this.socketfactory = sslContext.getSocketFactory();
        this.hostnameVerifier = BROWSER_COMPATIBLE_HOSTNAME_VERIFIER;
        this.nameResolver = nameResolver;
    }

    /**
     * @since 4.1
     */
    public SSLSocketFactory(
            final SSLContext sslContext, final X509HostnameVerifier hostnameVerifier) {
        super();
        Args.notNull(sslContext, "SSL context");
        this.socketfactory = sslContext.getSocketFactory();
        this.hostnameVerifier = hostnameVerifier;
        this.nameResolver = null;
    }

    /**
     * @since 4.2
     */
    public SSLSocketFactory(
            final javax.net.ssl.SSLSocketFactory socketfactory,
            final X509HostnameVerifier hostnameVerifier) {
        Args.notNull(socketfactory, "SSL socket factory");
        this.socketfactory = socketfactory;
        this.hostnameVerifier = hostnameVerifier;
        this.nameResolver = null;
    }

    /**
     * @param params Optional parameters. Parameters passed to this method will have no effect.
     *               This method will create a unconnected instance of {@link Socket} class.
     * @since 4.1
     *
     * @deprecated (4.3) use {@link #createSocket(HttpContext)}
     */
    @Deprecated
    public Socket createSocket(final HttpParams params) throws IOException {
        return createSocket((HttpContext) null);
    }

    /**
     * @deprecated (4.1) use {@link #createSocket(HttpParams)}
     */
    @Deprecated
    public Socket createSocket() throws IOException {
        return createSocket((HttpContext) null);
    }

    /**
     * @since 4.1
     *
     * @deprecated (4.3) use {@link #connectSocket(int, Socket, HttpHost, InetSocketAddress,
     *  InetSocketAddress, HttpContext)}
     */
    @Deprecated
    public Socket connectSocket(
            final Socket socket,
            final InetSocketAddress remoteAddress,
            final InetSocketAddress localAddress,
            final HttpParams params) throws IOException, UnknownHostException, ConnectTimeoutException {
        Args.notNull(remoteAddress, "Remote address");
        Args.notNull(params, "HTTP parameters");
        HttpHost host;
        if (remoteAddress instanceof HttpInetSocketAddress) {
            host = ((HttpInetSocketAddress) remoteAddress).getHttpHost();
        } else {
            host = new HttpHost(remoteAddress.getHostName(), remoteAddress.getPort(), "https");
        }
        final int connectTimeout = HttpConnectionParams.getConnectionTimeout(params);
        return connectSocket(connectTimeout, socket, host, remoteAddress, localAddress, null);
    }

    /**
     * Checks whether a socket connection is secure.
     * This factory creates TLS/SSL socket connections
     * which, by default, are considered secure.
     * <br/>
     * Derived classes may override this method to perform
     * runtime checks, for example based on the cypher suite.
     *
     * @param sock      the connected socket
     *
     * @return  <code>true</code>
     *
     * @throws IllegalArgumentException if the argument is invalid
     *
     * @deprecated (4.3) no longer used.
     */
    @Deprecated
    public boolean isSecure(final Socket sock) throws IllegalArgumentException {
        Args.notNull(sock, "Socket");
        Asserts.check(sock instanceof SSLSocket, "Socket not created by this factory");
        Asserts.check(!sock.isClosed(), "Socket is closed");
        return true;
    }

    /**
     * @since 4.2
     *
     * @deprecated (4.3) use {@link #createLayeredSocket(Socket, String, int, HttpContext)}
     */
    @Deprecated
    public Socket createLayeredSocket(
        final Socket socket,
        final String host,
        final int port,
        final HttpParams params) throws IOException, UnknownHostException {
        return createLayeredSocket(socket, host, port, (HttpContext) null);
    }

    /**
     * @deprecated (4.1) use {@link #createLayeredSocket(Socket, String, int, HttpParams)}
     */
    @Deprecated
    public Socket createLayeredSocket(
        final Socket socket,
        final String host,
        final int port,
        final boolean autoClose) throws IOException, UnknownHostException {
        return createLayeredSocket(socket, host, port, (HttpContext) null);
    }

    /**
     * @deprecated (4.1) use constructor.
     */
    @Deprecated
    public void setHostnameVerifier(final X509HostnameVerifier hostnameVerifier) {
        Args.notNull(hostnameVerifier, "Hostname verifier");
        this.hostnameVerifier = hostnameVerifier;
    }

    public X509HostnameVerifier getHostnameVerifier() {
        return this.hostnameVerifier;
    }

    /**
     * @deprecated (4.1) Use {@link #connectSocket(Socket, InetSocketAddress, InetSocketAddress,
     *   HttpParams)}
     */
    @Deprecated
    public Socket connectSocket(
            final Socket socket,
            final String host, final int port,
            final InetAddress local, int localPort,
            final HttpParams params) throws IOException, UnknownHostException, ConnectTimeoutException {
        InetAddress remote;
        if (this.nameResolver != null) {
            remote = this.nameResolver.resolve(host);
        } else {
            remote = InetAddress.getByName(host);
        }
        InetSocketAddress localAddress = null;
        if (local != null || localPort > 0) {
            // we need to bind explicitly
            if (localPort < 0) {
                localPort = 0; // indicates "any"
            }
            localAddress = new InetSocketAddress(local, localPort);
        }
        final InetSocketAddress remoteAddress = new HttpInetSocketAddress(
                new HttpHost(host, port), remote, port);
        return connectSocket(socket, remoteAddress, localAddress, params);
    }

    /**
     * @deprecated (4.1) Use {@link #createLayeredSocket(Socket, String, int, boolean)}
     */
    @Deprecated
    public Socket createSocket(
            final Socket socket,
            final String host, final int port,
            final boolean autoClose) throws IOException, UnknownHostException {
        return createLayeredSocket(socket, host, port, autoClose);
    }

    /**
     * Performs any custom initialization for a newly created SSLSocket
     * (before the SSL handshake happens).
     *
     * The default implementation is a no-op, but could be overriden to, e.g.,
     * call {@link SSLSocket#setEnabledCipherSuites(java.lang.String[])}.
     *
     * @since 4.2
     */
    protected void prepareSocket(final SSLSocket socket) throws IOException {
    }

    /**
     * {@inheritDoc}
     *
     * @since 4.3
     */
    public Socket createSocket(final HttpContext context) throws IOException {
        final SSLSocket sock = (SSLSocket) this.socketfactory.createSocket();
        prepareSocket(sock);
        return sock;
    }

    /**
     * {@inheritDoc}
     *
     * @since 4.3
     */
    public Socket connectSocket(
            final int connectTimeout,
            final Socket socket,
            final HttpHost host,
            final InetSocketAddress remoteAddress,
            final InetSocketAddress localAddress,
            final HttpContext context) throws IOException, ConnectTimeoutException {
        Args.notNull(host, "HTTP host");
        Args.notNull(remoteAddress, "Remote address");
        Socket sock = socket != null ? socket : createSocket(context);
        if (localAddress != null) {
            sock.bind(localAddress);
        }
        try {
            sock.connect(remoteAddress, connectTimeout);
        } catch (final SocketTimeoutException ex) {
            throw new ConnectTimeoutException(host, remoteAddress);
        }
        // Setup SSL layering if necessary
        if (sock instanceof SSLSocket) {
            verifyHostname((SSLSocket) sock, host.getHostName());
        } else {
            sock = createLayeredSocket(sock, host.getHostName(), remoteAddress.getPort(), context);
        }
        return sock;
    }

    public Socket createLayeredSocket(
            final Socket socket,
            final String target,
            final int port,
            final HttpContext context) throws IOException, UnknownHostException {
        final SSLSocket sslSocket = (SSLSocket) this.socketfactory.createSocket(
                socket,
                target,
                port,
                true);
          prepareSocket(sslSocket);
          verifyHostname(sslSocket, target);
          return sslSocket;
    }

    private void verifyHostname(final SSLSocket sslsock, final String hostname) throws IOException {
        if (this.hostnameVerifier != null) {
            try {
                this.hostnameVerifier.verify(hostname, sslsock);
                // verifyHostName() didn't blowup - good!
            } catch (final IOException iox) {
                // close the socket before re-throwing the exception
                try { sslsock.close(); } catch (final Exception x) { /*ignore*/ }
                throw iox;
            }
        }
    }

}
