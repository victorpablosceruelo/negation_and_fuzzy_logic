/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/ConnectionInterceptor.java,v 1.5 2001/09/01 21:40:45 remm Exp $
 * $Revision: 1.5 $
 * $Date: 2001-09-01 23:40:46 +0200 (Sat, 01 Sep 2001) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999 The Apache Software Foundation.  All rights 
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

package org.apache.commons.httpclient;

import java.util.Hashtable;

/**
 * The connection interceptor.
 * 
 * @author Remy Maucherat
 */

public interface ConnectionInterceptor {


    // --------------------------------------------------------- Public Methods


    /**
     * Connect.
     */
    public void connect();


    /**
     * Disconnect.
     */
    public void disconnect();


    /**
     * Retry.
     * 
     * @return boolean true if a retry should be attempted
     */
    public boolean retry(int status);


    /**
     * Recieved an informational status code.
     * 
     * @return boolean true if a retry should be attempted
     */
    public boolean info(int status, Hashtable headers);


    /**
     * Unexpected error.
     * 
     * @param status Status code; can be equal to -1 if status code is not 
     * known
     * @param e Underlying exception; can be null
     * @return boolean true if processing of the request should be stopped
     */
    public boolean error(int status, Exception e);


    /**
     * Sent request.
     * 
     * @param requestLine Request line sent
     * @param headers Headers sent
     */
    public void sentRequest();


    /**
     * Received response.
     * 
     * @param responseLine Response line recieved
     * @param headers Headers recieved
     */
    public void receivedResponse();


    /**
     * Received expectation.
     */
    public void receivedExpectation();


    /**
     * Required authentication.
     */
    public void requiredAuthentication();


    /**
     * Authenticate.
     */
    public void authenticate();


}
