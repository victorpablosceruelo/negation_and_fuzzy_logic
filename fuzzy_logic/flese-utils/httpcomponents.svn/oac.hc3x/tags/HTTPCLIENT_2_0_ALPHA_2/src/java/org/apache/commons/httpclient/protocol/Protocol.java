/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/protocol/Protocol.java,v 1.3 2003/01/23 22:48:21 jsdever Exp $
 * $Revision: 1.3 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
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
 * 4. The names "The Jakarta Project", "Tomcat", and "Apache Software
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
package org.apache.commons.httpclient.protocol;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * A class to encapsulate the specifics of a protocol.  This class class also
 * provides the ability to customize the set and characteristics of the
 * protocols used.
 * 
 * <p>One use case for modifying the default set of protocols would be to set a
 * custom SSL socket factory.  This would look something like the following:
 * <pre> 
 * Protocol myHTTPS = new Protocol( "https", new MySSLSocketFactory(), 443 );
 * 
 * Protocol.registerProtocol( "https", myHTTPS );
 * </pre>
 *
 * @author Michael Becke 
 * @author Jeff Dever
 *  
 * @since 2.0 
 */
public class Protocol {

    private static Map protocols = Collections.synchronizedMap( new HashMap() );

    /**
     * Registers a new protocol with the given identifier.  If a protocol with
     * the given ID already exists it will be overridden.  This ID is the same
     * one used to retrieve the protocol from getProtocol(String).
     * 
     * @param id the identifier for this protocol
     * @param protocol the protocol to register
     * 
     * @see #getProtocol(String)
     */
    public static void registerProtocol( String id, Protocol protocol ) {

        if ( id == null ) {
            throw new IllegalArgumentException( "id is null" );
        }
        if ( protocol == null ) {
            throw new IllegalArgumentException( "protocol is null" );
        }

        protocols.put( id, protocol );

    }

    /**
     * Unregisters the protocol with the given ID.
     * 
     * @param id the ID of the protocol to remove
     */
    public static void unregisterProtocol( String id ) {

        if ( id == null ) {
            throw new IllegalArgumentException( "id is null" );
        }

        protocols.remove( id );

    }

    /**
     * Gets the protocol with the given ID.
     * 
     * @param id the protocol ID
     * 
     * @return Protocol a protocol
     * 
     * @throws IllegalStateException if a protocol with the ID cannot be found
     */
    public static Protocol getProtocol( String id ) {

        if ( id == null ) {
            throw new IllegalArgumentException( "id is null" );
        }

        Protocol protocol = (Protocol)protocols.get( id );

        if ( protocol == null ) {
            protocol = lazyRegisterProtocol(id);
        }

        return protocol;

    } 

    /**
     * Lazily registers the protocol with the given id.
     * 
     * @param id the protocol ID
     * 
     * @return the lazily registered protocol
     * 
     * @throws IllegalStateException if the protocol with id is not recognized
     */
    private static Protocol lazyRegisterProtocol(String id) {

        if ("http".equals(id)) {
            Protocol HTTP = new Protocol(
                    "http",
                    new DefaultProtocolSocketFactory(),
                    80
                    );
            Protocol.registerProtocol( "http", HTTP );
            return HTTP;
        }

        if ("https".equals(id)) {
            Protocol HTTPS = new Protocol(
                    "https",
                    new SSLProtocolSocketFactory(),
                    443
                    );
            Protocol.registerProtocol( "https", HTTPS );
            return HTTPS;
        }

        throw new IllegalStateException( "unsupported protocol: '" + id + "'");

    }
    

    /** the scheme of this protocol (e.g. http, https) */
    private String scheme;
    
    private ProtocolSocketFactory socketFactory;
    
    private int defaultPort;
    
    private boolean secure;
  
    /**
     * Constructs a new Protocol.  The created protcol is insecure.
     * 
     * @param scheme the scheme (e.g. http, https)
     * @param factory the factory for creating sockets for communication using
     * this protocol
     * @param defaultPort the port this protocol defaults to
     */
    public Protocol( String scheme, ProtocolSocketFactory factory, int defaultPort ) {
        
        if ( scheme == null ) {
            throw new IllegalArgumentException( "scheme is null" );
        }
        if ( factory == null ) {
            throw new IllegalArgumentException( "socketFactory is null" );
        }
        if ( defaultPort <= 0 ) {
            throw new IllegalArgumentException( "port is invalid: " + defaultPort );
        }
        
        this.scheme = scheme;
        this.socketFactory = factory;
        this.defaultPort = defaultPort;
        this.secure = false;
        
    }
    
    /**
     * Constructs a new Protocol.  The created protcol is secure.
     *
     * @param scheme the scheme (e.g. http, https)
     * @param factory the factory for creating sockets for communication using
     * this protocol
     * @param defaultPort the port this protocol defaults to
     */
    public Protocol( String scheme, SecureProtocolSocketFactory factory, int defaultPort ) {
        
        if ( scheme == null ) {
            throw new IllegalArgumentException( "scheme is null" );
        }
        if ( factory == null ) {
            throw new IllegalArgumentException( "socketFactory is null" );
        }
        if ( defaultPort <= 0 ) {
            throw new IllegalArgumentException( "port is invalid: " + defaultPort );
        }

        this.scheme = scheme;
        this.socketFactory = factory;
        this.defaultPort = defaultPort;
        this.secure = true;        
        
    }

    /**
     * Returns the defaultPort.
     * @return int
     */
    public int getDefaultPort() {
        return defaultPort;
    }

    /**
     * Returns the socketFactory.  If secure the factory is a
     * SecureProtocolSocketFactory.
     * @return SocketFactory
     */
    public ProtocolSocketFactory getSocketFactory() {
        return socketFactory;
    }

    /**
     * Returns the scheme.
     * @return String
     */
    public String getScheme() {
        return scheme;
    }

    /**
     * Returns the secure.
     * @return boolean
     */
    public boolean isSecure() {
        return secure;
    }
    
    /**
     * Resolves the correct port for this protocol.  Returns the given port if
     * valid or the default port otherwise.
     * 
     * @param port the port to be resolved
     * 
     * @return the given port or the defaultPort
     */
    public int resolvePort( int port ) {
        return port <= 0 ? getDefaultPort() : port;
    }

    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return scheme + ":" + defaultPort;
    }
    
    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        
        if ( obj instanceof Protocol ) {
            
            Protocol p = (Protocol)obj;
            
            return (
                defaultPort == p.getDefaultPort()
                && scheme.equalsIgnoreCase( p.getScheme() )
                && secure == p.isSecure()
                && socketFactory.equals( p.getSocketFactory() )
            );
            
        } else {
            return false;
        }
        
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return scheme.hashCode();
    }

}
