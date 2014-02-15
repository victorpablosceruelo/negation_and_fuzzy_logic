/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/DefaultMethodRetryHandler.java,v 1.3 2004/04/18 23:51:34 jsdever Exp $
 * $Revision: 1.3 $
 * $Date: 2004-04-19 01:51:38 +0200 (Mon, 19 Apr 2004) $
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
 * The default MethodRetryHandler used by HttpMethodBase.
 * 
 * @author Michael Becke
 * 
 * @see HttpMethodBase#setMethodRetryHandler(MethodRetryHandler)
 */
public class DefaultMethodRetryHandler implements MethodRetryHandler {

    /** the number of times a method will be retried */
    private int retryCount;
    
    /** Whether or not methods that have successfully sent their request will be retried */
    private boolean requestSentRetryEnabled;
    
    /**
     */
    public DefaultMethodRetryHandler() {
        this.retryCount = 3;
        this.requestSentRetryEnabled = false;
    }
    
    /** 
     * Used <code>retryCount</code> and <code>requestSentRetryEnabled</code> to determine
     * if the given method should be retried.
     * 
     * @see MethodRetryHandler#retryMethod(HttpMethod, HttpConnection, HttpRecoverableException, int, boolean)
     */
    public boolean retryMethod(
        HttpMethod method,
        HttpConnection connection,
        HttpRecoverableException recoverableException,
        int executionCount,
        boolean requestSent
    ) {
        return ((!requestSent || requestSentRetryEnabled) && (executionCount <= retryCount));
    }
    /**
     * @return <code>true</code> if this handler will retry methods that have 
     * successfully sent their request, <code>false</code> otherwise
     */
    public boolean isRequestSentRetryEnabled() {
        return requestSentRetryEnabled;
    }

    /**
     * @return the maximum number of times a method will be retried
     */
    public int getRetryCount() {
        return retryCount;
    }

    /**
     * @param requestSentRetryEnabled a flag indicating if methods that have 
     * successfully sent their request should be retried
     */
    public void setRequestSentRetryEnabled(boolean requestSentRetryEnabled) {
        this.requestSentRetryEnabled = requestSentRetryEnabled;
    }

    /**
     * @param retryCount the maximum number of times a method can be retried
     */
    public void setRetryCount(int retryCount) {
        this.retryCount = retryCount;
    }

}
