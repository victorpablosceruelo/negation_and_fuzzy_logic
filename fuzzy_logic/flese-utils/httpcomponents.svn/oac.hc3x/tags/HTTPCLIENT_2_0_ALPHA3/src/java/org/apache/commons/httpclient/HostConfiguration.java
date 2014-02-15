/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/HostConfiguration.java,v 1.8 2003/01/28 04:40:20 jsdever Exp $
 * $Revision: 1.8 $
 * $Date: 2003-01-28 05:40:23 +0100 (Tue, 28 Jan 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient;

import org.apache.commons.httpclient.protocol.Protocol;

/**
 *
 * @author <a href="mailto:becke@u.washington.edu">Michael Becke</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @since 2.0 
 */
public class HostConfiguration implements Cloneable {

    /** The host to use. */
    private String host;

    /** The port to use. */
    private int port;

    /** The protocol */
    private Protocol protocol;

    /** True if a host has been set */
    private boolean hostSet;

    /** The host name of the proxy server */
    private String proxyHost;

    /** The port number of the proxy server */
    private int proxyPort;

    /** True if a proxy server has been set */
    private boolean proxySet;
    
    /**
     * Constructor for HostConfiguration.
     */
    public HostConfiguration() {
        
        this.host = null;
        this.port = -1;
        this.protocol = null;
        this.hostSet = false;
        
        this.proxyHost = null;
        this.proxyPort = -1;
        this.proxySet = false;
        
    }
    
    /**
     * Copy constructor for HostConfiguration
     * 
     * @param hostConfiguration the hostConfiguration to copy
     */
    public HostConfiguration (HostConfiguration hostConfiguration) {
        
        // wrap all of the assignments in a synchronized block to avoid
        // having to negotiate the monitor for each method call
        synchronized (hostConfiguration) {            
            this.host = hostConfiguration.getHost();
            this.port = hostConfiguration.getPort();
            this.protocol = hostConfiguration.getProtocol();
            this.hostSet = hostConfiguration.isHostSet();
            
            this.proxyHost = hostConfiguration.getProxyHost();
            this.proxyPort = hostConfiguration.getProxyPort();
            this.proxySet = hostConfiguration.isProxySet();
        }
        
    }

    /**
     * @see java.lang.Object#clone()
     */
    public Object clone() {
        return new HostConfiguration(this);
    }
    
    /**
     * Tests if the host configuration equals the configuraiton set on the
     * connection. True only if the host, port and protocol are equal.  If no
     * host configuration has been set false will be returned.
     * 
     * @param connection the connection to test against
     * @return true if the connection's host information equals that of this
     * configuration
     * 
     * @see #proxyEquals(HttpConnection)
     */
    public synchronized boolean hostEquals(HttpConnection connection) {
        
        if (hostSet) {
            return (host.equalsIgnoreCase(connection.getHost())
                && port == connection.getPort()
                && protocol.equals(connection.getProtocol()));
            
        } else {
            return false;   
        }
        
    }

    /**
     * Tests if the proxy configuration equals the configuraiton set on the
     * connection. True only if the proxyHost and proxyPort are equal.
     *
     * @param connection the connection to test against
     * @return true if the connection's proxy information equals that of this
     * configuration
     *
     * @see #hostEquals(HttpConnection)
     */
    public synchronized boolean proxyEquals(HttpConnection connection) {
        
        if (proxyHost == null) {
            return connection.getProxyHost() == null;   
        } else {
            return (
                proxyHost.equalsIgnoreCase(connection.getProxyHost())
                && proxyPort == connection.getProxyPort()
            );
        }
        
    }    
    
    /**
     * Return true if the host is set.
     * @return boolean True if the host is set.
     */
    public synchronized boolean isHostSet() {
        return hostSet;
    }

    /**
     *  Set the host
     * @param host The host.
     * @param port The port
     * @param protocol The protocol.
     */
    public synchronized void setHost(String host, int port, String protocol) {
        setHost(host, port, Protocol.getProtocol(protocol));
    }
    
    /**
     * Sets this configuration's host infomation.
     * 
     * @param host the host, IP or DNS name
     * @param port the host port or -1 to use protocol default
     * @param protocol the protocol
     */
    public synchronized void setHost(String host, int port, 
        Protocol protocol) {
            
        if (host == null) {
            throw new IllegalArgumentException("host must not be null");   
        }
        if (protocol == null) {
            throw new IllegalArgumentException("protocol must not be null");   
        }
        
        this.host = host;
        this.port = port == -1 ? protocol.getDefaultPort() : port;
        this.protocol = protocol;
        
        this.hostSet = true;
        
    }

    /**
     * Sets the protocol, host and port from the given URI.
     * @param uri the URI.
     */
    public synchronized void setHost(URI uri) {
        try {
            setHost(uri.getHost(), uri.getPort(), uri.getScheme());
        } catch (URIException e) {
            throw new IllegalArgumentException(e.toString());
        }
    }

    /**
     * Return the host url.
     * 
     * @return String The host url.
     */
    public synchronized String getHostURL() {
        
        if (!hostSet) {
            throw new IllegalStateException("a default host must be set to "
                + "create a host URL" 
            );   
        }
        
        String url = protocol.getScheme() + "://" + host;
        
        if (port != -1 && port != protocol.getDefaultPort()) {
            url += ":" + port;
        }
        
        return url;
        
    }

    /**
     * Returns the host.
     * @return String
     */
    public synchronized String getHost() {
        return host;
    }

    /**
     * Returns the port.
     * @return int
     */
    public synchronized int getPort() {
        return port;
    }

    /**
     * Returns the protocol.
     * @return String The protocol.
     */
    public synchronized Protocol getProtocol() {
        return protocol;
    }

    /**
     * @return boolean True if a proxy server has been set.
     */    
    public synchronized boolean isProxySet() {
        return proxySet;   
    }

    /**
     * Set the proxy settings.
     * @param proxyHost The proxy host
     * @param proxyPort The proxy port
     */
    public synchronized void setProxy(String proxyHost, int proxyPort) {
        
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        
        this.proxySet = true;
    }

    /**
     * Returns the proxyHost.
     * @return String
     */
    public synchronized String getProxyHost() {
        return proxyHost;
    }

    /**
     * Returns the proxyPort.
     * @return int
     */
    public synchronized int getProxyPort() {
        return proxyPort;
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public synchronized boolean equals(Object o) {
        
        if (o instanceof HostConfiguration) {
            
            // shortcut if we're comparing with ourselves
            if (o == this) { 
                return true;
            }
            
            HostConfiguration config = (HostConfiguration) o;
            
            if (hostSet) {
                if (!host.equalsIgnoreCase(config.getHost())
                    || port != config.getPort()
                    || !protocol.equals(config.getProtocol())) {
                    // either host, port or protocol don't match
                    return false;
                }
            } else if (config.isHostSet()) {
                return false;
            }
            if (proxyHost != null) {
                if (!proxyHost.equalsIgnoreCase (config.getProxyHost())
                    || proxyPort != config.getProxyPort()) {
                    // either proxyHost or proxyPort don't match
                    return false;
                }
            } else if (config.getProxyHost() != null) {
                return false;
            }            

            // everything matches
            return true;

        } else {
            return false;
        }
        
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        
        if (host != null) {
            return host.hashCode();
        } else if (proxyHost != null) {
            return proxyHost.hashCode();   
        } else {
            return super.hashCode();
        }
    }
}
