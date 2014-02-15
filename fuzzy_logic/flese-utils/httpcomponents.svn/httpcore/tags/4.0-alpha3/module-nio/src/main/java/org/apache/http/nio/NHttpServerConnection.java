/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-nio/src/main/java/org/apache/http/nio/NHttpServerConnection.java $
 * $Revision: 473994 $
 * $Date: 2006-11-12 18:18:52 +0100 (Sun, 12 Nov 2006) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2006 The Apache Software Foundation
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

package org.apache.http.nio;

import org.apache.http.HttpException;
import org.apache.http.HttpResponse;

/**
 * Abstract non-blocking server-side HTTP connection. It can be used to
 * receive HTTP requests and asynchronously submit HTTP responses. 
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public interface NHttpServerConnection extends NHttpConnection {

    /**
     * Submits the HTTP response to the client.
     *  
     * @param response HTTP response
     * 
     * @throws HttpException if the HTTP response violates the HTTP protocol.
     */
    void submitResponse(HttpResponse response) throws HttpException;

    /**
     * Returns <tt>true</tt> if an HTTP response has been submitted to the 
     * client.
     * 
     * @return <tt>true</tt> if an HTTP response has been submitted, 
     * <tt>false</tt> otherwise. 
     */
    boolean isResponseSubmitted();
    
}
