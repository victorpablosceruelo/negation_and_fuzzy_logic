/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/ErrorResponse.java,v 1.1.2.2 2004/02/22 18:21:18 olegk Exp $
 * $Revision: 1.1.2.2 $
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
    }
    
    public void setResponse(int statusCode, GenericResponse r) {
        Integer code = new Integer(statusCode);
        responses.put(code, r);
    }
    
    public GenericResponse getResponse(int statusCode) {
        Integer code = new Integer(statusCode);
        GenericResponse r = (GenericResponse)responses.get(code);
        if(r == null) {
            String text = statusCode+" "+HttpStatus.getStatusText(statusCode);
            r = new GenericResponse("HTTP/1.0 "+text,
            "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n"
                + "<html>\n<head>"
                + "<title>"+text+"</title>"
                + "</head>\n<body>"
                + "<h1>"+text+"</h1></body>\n</html>\n",
            "text/html");
            
            responses.put(code, r);
        }
        return r;
    }
}
