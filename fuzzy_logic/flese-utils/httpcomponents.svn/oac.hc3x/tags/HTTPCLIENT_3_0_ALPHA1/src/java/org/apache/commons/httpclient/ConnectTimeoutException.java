/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/ConnectTimeoutException.java,v 1.4 2004/05/13 04:03:25 mbecke Exp $
 * $Revision: 1.4 $
 * $Date: 2004-05-13 06:03:25 +0200 (Thu, 13 May 2004) $
 *
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

/**
 * A timeout while connecting to an HTTP server or waiting for an
 * available connection from an HttpConnectionManager.
 * 
 * @author <a href="mailto:laura@lwerner.org">Laura Werner</a>
 * 
 * @since 3.0
 */
public class ConnectTimeoutException extends HttpTimeoutException {

    /**
     * Creates a ConnectTimeoutException with a <tt>null</tt> detail message.
     */
    public ConnectTimeoutException() {
        super();
    }

    /**
     * Creates a ConnectTimeoutException with the specified detail message.
     * 
     * @param message The exception detail message 
     */
    public ConnectTimeoutException(String message) {
        super(message);
    }

    /**
     * Creates a new ConnectTimeoutException with the specified detail message and cause.
     * 
     * @param message the exception detail message
     * @param cause the <tt>Throwable</tt> that caused this exception, or <tt>null</tt>
     * if the cause is unavailable, unknown, or not a <tt>Throwable</tt>
     */
    public ConnectTimeoutException(String message, Throwable cause) {
        super(message, cause);
    }

}
