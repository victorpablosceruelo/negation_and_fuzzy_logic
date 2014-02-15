/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/protocol/SSLProtocolSocketFactory.java,v 1.5.2.2 2004/02/22 18:21:16 olegk Exp $
 * $Revision: 1.5.2.2 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
 * ====================================================================
 *
 *  Copyright 2002-2004 The Apache Software Foundation
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient.protocol;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

import javax.net.ssl.SSLSocketFactory;

/**
 * A SecureProtocolSocketFactory that uses JSSE to create sockets.
 * 
 * @author Michael Becke
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * 
 * @since 2.0
 */
public class SSLProtocolSocketFactory implements SecureProtocolSocketFactory {

    /**
     * The factory singleton.
     */
    private static final SSLProtocolSocketFactory factory = new SSLProtocolSocketFactory();
    
    /**
     * Gets an singleton instance of the SSLProtocolSocketFactory.
     * @return a SSLProtocolSocketFactory
     */
    static SSLProtocolSocketFactory getSocketFactory() {
        return factory;
    }    
    
    /**
     * Constructor for SSLProtocolSocketFactory.
     */
    public SSLProtocolSocketFactory() {
        super();
    }

    /**
     * @see SecureProtocolSocketFactory#createSocket(java.lang.String,int,java.net.InetAddress,int)
     */
    public Socket createSocket(
        String host,
        int port,
        InetAddress clientHost,
        int clientPort)
        throws IOException, UnknownHostException {
        return SSLSocketFactory.getDefault().createSocket(
            host,
            port,
            clientHost,
            clientPort
        );
    }

    /**
     * @see SecureProtocolSocketFactory#createSocket(java.lang.String,int)
     */
    public Socket createSocket(String host, int port)
        throws IOException, UnknownHostException {
        return SSLSocketFactory.getDefault().createSocket(
            host,
            port
        );
    }

    /**
     * @see SecureProtocolSocketFactory#createSocket(java.net.Socket,java.lang.String,int,boolean)
     */
    public Socket createSocket(
        Socket socket,
        String host,
        int port,
        boolean autoClose)
        throws IOException, UnknownHostException {
        return ((SSLSocketFactory) SSLSocketFactory.getDefault()).createSocket(
            socket,
            host,
            port,
            autoClose
        );
    }

    /**
     * All instances of SSLProtocolSocketFactory are the same.
     */
    public boolean equals(Object obj) {
        return ((obj != null) && obj.getClass().equals(SSLProtocolSocketFactory.class));
    }

    /**
     * All instances of SSLProtocolSocketFactory have the same hash code.
     */
    public int hashCode() {
        return SSLProtocolSocketFactory.class.hashCode();
    }    
    
}
