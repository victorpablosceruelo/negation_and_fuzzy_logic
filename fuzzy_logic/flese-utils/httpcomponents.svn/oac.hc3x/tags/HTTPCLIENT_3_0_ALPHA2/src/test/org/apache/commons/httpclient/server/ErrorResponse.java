/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/ErrorResponse.java,v 1.4 2004/02/27 19:01:33 olegk Exp $
 * $Revision: 1.4 $
 * $Date: 2004-02-27 20:01:34 +0100 (Fri, 27 Feb 2004) $
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

package org.apache.commons.httpclient.server;

import java.util.HashMap;

import org.apache.commons.httpclient.HttpStatus;

/**
 * Default error responses.
 * 
 * @author Christian Kohlschuetter
 */
public class ErrorResponse {
    private static ErrorResponse instance = null;
    public static synchronized ErrorResponse getInstance() {
        if(instance == null) {
            instance = new ErrorResponse();
        }
        return instance;
    }
    
    private final HashMap responses = new HashMap();
    
    private ErrorResponse() {
        super();
    }
    
    public void setResponse(int statusCode, SimpleResponse response) {
        Integer code = new Integer(statusCode);
        responses.put(code, response);
    }
    
    public SimpleResponse getResponse(int statusCode) {
        Integer code = new Integer(statusCode);
        SimpleResponse response = (SimpleResponse)responses.get(code);
        if (response == null) {
            StringBuffer buffer = new StringBuffer();
            buffer.append(statusCode);
            String s = HttpStatus.getStatusText(statusCode);
            if (s != null) {
                buffer.append(' ');
                buffer.append(s);
            }
            response = new SimpleResponse(buffer.toString());
            response.setContentType("text/plain");
            if (s == null) {
                s = "Error " + code;     
            }
            response.setBodyString(s);
            responses.put(code, response);
        }
        return response;
    }
}
