/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/main/java/org/apache/http/conn/HttpRoute.java $
 * $Revision: 543733 $
 * $Date: 2007-06-02 14:05:18 +0200 (Sat, 02 Jun 2007) $
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

package org.apache.http.conn;

import java.net.InetAddress;

import org.apache.http.HttpHost;
import org.apache.http.util.CharArrayBuffer;


/**
 * The route for a request.
 * Instances of this class are unmodifiable and therefore suitable
 * for use as lookup keys.
 * 
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines to avoid svn diff problems -->
 * @version $Revision: 543733 $
 *
 * @since 4.0
 */
public final class HttpRoute implements Cloneable {

    /** The target host to connect to. */
    private final HttpHost targetHost;

    /**
     * The local address to connect from.
     * <code>null</code> indicates that the default should be used.
     */
    private final InetAddress localAddress;

    /** The proxy servers, if any. */
    private final HttpHost[] proxyChain;

    /** Whether the the route is tunnelled through the proxy. */
    private final boolean tunnelled;

    /** Whether the route is layered. */
    private final boolean layered;

    /** Whether the route is (supposed to be) secure. */
    private final boolean secure;


    /**
     * Internal, fully-specified constructor.
     * This constructor does <i>not</i> clone the proxy chain array,
     * nor test it for <code>null</code> elements. This conversion and
     * check is the responsibility of the public constructors.
     * The order of arguments here is different from the similar public
     * constructor, as required by Java.
     *
     * @param local     the local address to route from, or
     *                  <code>null</code> for the default
     * @param target    the host to which to route
     * @param proxies   the proxy chain to use, or
     *                  <code>null</code> for a direct route
     * @param secure    <code>true</code> if the route is (to be) secure,
     *                  <code>false</code> otherwise
     * @param tunnelled <code>true</code> if the route is (to be) tunnelled
     *                  end-to-end via the proxy chain,
     *                  <code>false</code> otherwise
     * @param layered   <code>true</code> if the route includes a
     *                  layered protocol,
     *                  <code>false</code> otherwise
     */
    private HttpRoute(InetAddress local, HttpHost target, HttpHost[] proxies,
                      boolean secure, boolean tunnelled, boolean layered) {
        if (target == null) {
            throw new IllegalArgumentException
                ("Target host may not be null.");
        }
        if (tunnelled && (proxies == null)) {
            throw new IllegalArgumentException
                ("Proxy required if tunnelled.");
        }

        this.targetHost   = target;
        this.localAddress = local;
        this.proxyChain   = proxies;
        this.secure       = secure;
        this.tunnelled    = tunnelled;
        this.layered      = layered;
    }


    /**
     * Creates a new route with all attributes specified explicitly.
     *
     * @param target    the host to which to route
     * @param local     the local address to route from, or
     *                  <code>null</code> for the default
     * @param proxies   the proxy chain to use, or
     *                  <code>null</code> for a direct route
     * @param secure    <code>true</code> if the route is (to be) secure,
     *                  <code>false</code> otherwise
     * @param tunnelled <code>true</code> if the route is (to be) tunnelled
     *                  end-to-end via the proxy chain,
     *                  <code>false</code> otherwise
     * @param layered   <code>true</code> if the route includes a
     *                  layered protocol,
     *                  <code>false</code> otherwise
     */
    public HttpRoute(HttpHost target, InetAddress local, HttpHost[] proxies,
                     boolean secure, boolean tunnelled, boolean layered) {
        this(local, target, toChain(proxies), secure, tunnelled, layered);
    }


    /**
     * Creates a new route with at most one proxy.
     *
     * @param target    the host to which to route
     * @param local     the local address to route from, or
     *                  <code>null</code> for the default
     * @param proxy     the proxy to use, or
     *                  <code>null</code> for a direct route
     * @param secure    <code>true</code> if the route is (to be) secure,
     *                  <code>false</code> otherwise
     * @param tunnelled <code>true</code> if the route is (to be) tunnelled
     *                  via the proxy,
     *                  <code>false</code> otherwise
     * @param layered   <code>true</code> if the route includes a
     *                  layered protocol,
     *                  <code>false</code> otherwise
     */
    public HttpRoute(HttpHost target, InetAddress local, HttpHost proxy,
                     boolean secure, boolean tunnelled, boolean layered) {
        this(local, target, toChain(proxy), secure, tunnelled, layered);
    }


    /**
     * Creates a new direct route.
     * That is a route without a proxy.
     *
     * @param target    the host to which to route
     * @param local     the local address to route from, or
     *                  <code>null</code> for the default
     * @param secure    <code>true</code> if the route is (to be) secure,
     *                  <code>false</code> otherwise
     */
    public HttpRoute(HttpHost target, InetAddress local, boolean secure) {
        this(local, target, null, secure, false, false);
    }


    /**
     * Creates a new direct insecure route.
     *
     * @param target    the host to which to route
     */
    public HttpRoute(HttpHost target) {
        this(null, target, null, false, false, false);
    }


    /**
     * Creates a new route through a proxy.
     * When using this constructor, the <code>proxy</code> MUST be given.
     * For convenience, it is assumed that a secure connection will be
     * layered over a tunnel through the proxy.
     *
     * @param target    the host to which to route
     * @param local     the local address to route from, or
     *                  <code>null</code> for the default
     * @param proxy     the proxy to use
     * @param secure    <code>true</code> if the route is (to be) secure,
     *                  <code>false</code> otherwise
     */
    public HttpRoute(HttpHost target, InetAddress local, HttpHost proxy,
                     boolean secure) {
        this(local, target, toChain(proxy), secure, secure, secure);
        if (proxy == null) {
            throw new IllegalArgumentException
                ("Proxy host may not be null.");
        }
    }


    /**
     * Helper to convert a proxy to a proxy chain.
     *
     * @param proxy     the only proxy in the chain, or <code>null</code>
     *
     * @return  a proxy chain array, or <code>null</code>
     */
    private static HttpHost[] toChain(HttpHost proxy) {
        if (proxy == null)
            return null;

        return new HttpHost[]{ proxy };
    }


    /**
     * Helper to duplicate and check a proxy chain.
     * An empty proxy chain is converted to <code>null</code>.
     *
     * @param proxies   the proxy chain to duplicate, or <code>null</code>
     *
     * @return  a new proxy chain array, or <code>null</code>
     */
    private static HttpHost[] toChain(HttpHost[] proxies) {
        if ((proxies == null) || (proxies.length < 1))
            return null;

        for (int i=0; i<proxies.length; i++) {
            if (proxies[i] == null)
                throw new IllegalArgumentException
                    ("Proxy chain may not contain null elements.");
        }

        // copy the proxy chain, the traditional way
        HttpHost[] result = new HttpHost[proxies.length];
        System.arraycopy(proxies, 0, result, 0, proxies.length);

        return result;
    }


    /**
     * Obtains the target host.
     * 
     * @return the target host
     */
    public final HttpHost getTargetHost() {
        return this.targetHost;
    }


    /**
     * Obtains the local address to connect from.
     * 
     * @return  the local address,
     *          or <code>null</code>
     */
    public final InetAddress getLocalAddress() {
        return this.localAddress;
    }


    /**
     * Obtains the number of hops in this route.
     * A direct route has one hop. A route through a proxy has two hops.
     * A route through a chain of <i>n</i> proxies has <i>n+1</i> hops.
     *
     * @return  the number of hops in this route
     */
    public final int getHopCount() {
        return (proxyChain == null) ? 1 : (proxyChain.length+1);
    }


    /**
     * Obtains the target of a hop in this route.
     * The target of the last hop is the {@link #getTargetHost target host},
     * the target of previous hops is the respective proxy in the chain.
     * For a route through exactly one proxy, target of hop 0 is the proxy
     * and target of hop 1 is the target host.
     *
     * @param hop       index of the hop for which to get the target,
     *                  0 for first
     *
     * @return  the target of the given hop
     *
     * @throws IllegalArgumentException
     *  if the argument is negative or not less than
     *  {@link #getHopCount getHopCount()}
     */
    public final HttpHost getHopTarget(int hop) {
        if (hop < 0)
            throw new IllegalArgumentException
                ("Hop index must not be negative: " + hop);
        final int hopcount = getHopCount();
        if (hop >= hopcount)
            throw new IllegalArgumentException
                ("Hop index " + hop +
                 " exceeds route length " + hopcount +".");

        HttpHost result = null;
        if (hop < hopcount-1)
            result = this.proxyChain[hop];
        else
            result = this.targetHost;

        return result;
    }


    /**
     * Obtains the first proxy host.
     * 
     * @return the first proxy in the proxy chain, or
     *         <code>null</code> if this route is direct
     */
    public final HttpHost getProxyHost() {
        return (this.proxyChain == null) ? null : this.proxyChain[0];
    }


    /**
     * Checks whether this route is tunnelled through a proxy.
     * If there is a proxy chain, only end-to-end tunnels are considered.
     *
     * @return  <code>true</code> if tunnelled end-to-end through at least
     *          one proxy,
     *          <code>false</code> otherwise
     */
    public final boolean isTunnelled() {
        return this.tunnelled;
    }


    /**
     * Checks whether this route includes a layered protocol.
     * In the presence of proxies, only layering over an end-to-end tunnel
     * is considered.
     *
     * @return  <code>true</code> if layered,
     *          <code>false</code> otherwise
     */
    public final boolean isLayered() {
        return this.layered;
    }


    /**
     * Checks whether this route is secure.
     *
     * @return  <code>true</code> if secure,
     *          <code>false</code> otherwise
     */
    public final boolean isSecure() {
        return this.secure;
    }


    /**
     * Converts to the {@link HostConfiguration traditional} interface.
     *
     * @return  a host configuration matching this route as good as possible
     *
     * @deprecated No replacement.
     *          This class will replace {@link HostConfiguration}
     *          where routes need to be represented. No conversion necessary.
     */
    public final HostConfiguration toHostConfig() {
        if ((this.proxyChain != null) && (this.proxyChain.length > 1)) {
            throw new IllegalStateException("Cannot convert proxy chain.");
        }
        return new HostConfiguration
            (this.targetHost, getProxyHost(), this.localAddress);
    }


    /**
     * Compares this route to another.
     *
     * @param o         the object to compare with
     *
     * @return  <code>true</code> if the argument is the same route,
     *          <code>false</code>
     */
    public final boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof HttpRoute))
            return false;

        HttpRoute that = (HttpRoute) o;
        boolean equal = this.targetHost.equals(that.targetHost);
        equal &=
            ( this.localAddress == that.localAddress) ||
            ((this.localAddress != null) &&
              this.localAddress.equals(that.localAddress));
        equal &=
            ( this.proxyChain        == that.proxyChain) ||
            ((this.proxyChain        != null) &&
             (that.proxyChain        != null) &&
             (this.proxyChain.length == that.proxyChain.length));
        // comparison of actual proxies follows below
        equal &=
            (this.secure    == that.secure) &&
            (this.tunnelled == that.tunnelled) &&
            (this.layered   == that.layered);

        // chain length has been compared above, now check the proxies
        if (equal && (this.proxyChain != null)) {
            for (int i=0; equal && (i<this.proxyChain.length); i++)
                equal = this.proxyChain[i].equals(that.proxyChain[i]);
        }

        return equal;
    }


    /**
     * Generates a hash code for this route.
     *
     * @return  the hash code
     */
    public final int hashCode() {

        int hc = this.targetHost.hashCode();

        if (this.localAddress != null)
            hc ^= localAddress.hashCode();
        if (this.proxyChain != null) {
            hc ^= proxyChain.length;
            for (int i=0; i<proxyChain.length; i++)
                hc ^= proxyChain[i].hashCode();
        }

        if (this.secure)
            hc ^= 0x11111111;
        if (this.tunnelled)
            hc ^= 0x22222222;
        if (this.layered)
            hc ^= 0x44444444;

        return hc;
    }


    /**
     * Obtains a description of this route.
     *
     * @return  a human-readable representation of this route
     */
    public final String toString() {
        CharArrayBuffer cab = new CharArrayBuffer(50 + getHopCount()*30);

        cab.append("HttpRoute[");
        if (this.localAddress != null) {
            cab.append(this.localAddress);
            cab.append("->");
        }
        cab.append('{');
        if (this.tunnelled)
            cab.append('t');
        if (this.layered)
            cab.append('l');
        if (this.secure)
            cab.append('s');
        cab.append("}->");
        if (this.proxyChain != null) {
            for (int i=0; i<this.proxyChain.length; i++) {
                cab.append(this.proxyChain[i]);
                cab.append("->");
            }
        }
        cab.append(this.targetHost);
        cab.append(']');

        return cab.toString();
    }


    // default implementation of clone() is sufficient
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }


} // class HttpRoute
