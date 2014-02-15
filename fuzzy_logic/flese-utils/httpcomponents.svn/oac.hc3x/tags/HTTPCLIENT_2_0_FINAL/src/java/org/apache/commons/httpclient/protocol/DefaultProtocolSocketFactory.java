/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/protocol/DefaultProtocolSocketFactory.java,v 1.5.2.1 2004/01/30 19:23:15 mbecke Exp $
 * $Revision: 1.5.2.1 $
 * $Date: 2004-01-30 20:23:15 +0100 (Fri, 30 Jan 2004) $
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

package org.apache.commons.httpclient.protocol;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * The default class for creating protocol sockets.  This class just uses the
 * {@link java.net.Socket socket} constructors.
 * 
 * @author Michael Becke
 * 
 * @since 2.0
 */
public class DefaultProtocolSocketFactory implements ProtocolSocketFactory {

    /**
     * The factory singleton.
     */
    private static final DefaultProtocolSocketFactory factory = new DefaultProtocolSocketFactory();
    
    /**
     * Gets an singleton instance of the DefaultProtocolSocketFactory.
     * @return a DefaultProtocolSocketFactory
     */
    static DefaultProtocolSocketFactory getSocketFactory() {
        return factory;
    }
    
    /**
     * Constructor for DefaultProtocolSocketFactory.
     */
    public DefaultProtocolSocketFactory() {
        super();
    }

    /**
     * @see #createSocket(java.lang.String,int,java.net.InetAddress,int)
     */
    public Socket createSocket(
        String host,
        int port,
        InetAddress clientHost,
        int clientPort
    ) throws IOException, UnknownHostException {
        return new Socket(host, port, clientHost, clientPort);
    }

    /**
     * @see ProtocolSocketFactory#createSocket(java.lang.String,int)
     */
    public Socket createSocket(String host, int port)
        throws IOException, UnknownHostException {
        return new Socket(host, port);
    }

    /**
     * All instances of DefaultProtocolSocketFactory are the same.
     */
    public boolean equals(Object obj) {
        return ((obj != null) && obj.getClass().equals(DefaultProtocolSocketFactory.class));
    }

    /**
     * All instances of DefaultProtocolSocketFactory have the same hash code.
     */
    public int hashCode() {
        return DefaultProtocolSocketFactory.class.hashCode();
    }

}
