/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/HttpTimeoutException.java,v 1.5 2004/05/13 04:03:25 mbecke Exp $
 * $Revision: 1.5 $
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
 * A timeout while processing an HTTP request: for example a network
 * timeout or a timeout while waiting for an HttpConnection to become
 * available.
 * 
 * @author <a href="mailto:laura@lwerner.org">Laura Werner</a>
 * 
 * @since 3.0
 */
public class HttpTimeoutException extends HttpRecoverableException {

    /**
     * Creates a new HttpTimeoutException with a <tt>null</tt> detail message.
     */
    public HttpTimeoutException() {
        super();
    }

    /**
     * Creates a new HttpTimeoutException with the specified detail message.
     * 
     * @param message The exception detail message
     */
    public HttpTimeoutException(String message) {
        super(message);
    }

    /**
     * Creates a new HttpTimeoutException with the specified detail message and cause.
     *
     * @param message the exception detail message
     * @param cause the <tt>Throwable</tt> that caused this exception, or <tt>null</tt>
     * if the cause is unavailable, unknown, or not a <tt>Throwable</tt>
     */
    public HttpTimeoutException(String message, Throwable cause) {
        super(message, cause);
    }

}
