/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/MethodRetryHandler.java,v 1.2.2.1 2004/02/22 18:21:13 olegk Exp $
 * $Revision: 1.2.2.1 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */

package org.apache.commons.httpclient;

/**
 * A handler for determining if an HttpMethod should be retried after a 
 * recoverable exception during execution.
 * 
 * @see HttpMethod#execute(HttpState, HttpConnection)
 * @see HttpRecoverableException
 * 
 * @author Michael Becke
 */
public interface MethodRetryHandler {

    /**
     * Determines if a method should be retried after an HttpRecoverableException
     * occurs during execution.
     * 
     * @param method the method being executed
     * @param connection the connection the method is using
     * @param recoverableException the exception that occurred
     * @param executionCount the number of times this method has been 
     * unsuccessfully executed
     * @param requestSent a flag indicating if the request has been fully sent or not
     * 
     * @return <code>true</code> if the method should be retried, <code>false</code>
     * otherwise
     */
    boolean retryMethod(
        HttpMethod method, 
        HttpConnection connection,
        HttpRecoverableException recoverableException,
        int executionCount,
        boolean requestSent);

}
