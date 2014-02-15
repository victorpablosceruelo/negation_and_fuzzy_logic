/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test-webapp/src/org/apache/commons/httpclient/RequestBodyServlet.java,v 1.5 2004/02/22 18:08:52 olegk Exp $
 * $Revision: 1.5 $
 * $Date: 2004-02-22 19:08:52 +0100 (Sun, 22 Feb 2004) $
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

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class RequestBodyServlet extends MultiMethodServlet {
    protected void genericService(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        response.setContentType("text/html");
        PrintWriter out = response.getWriter();
        StringBuffer buf = null;
        if(request.getContentLength() > 0) {
            buf = new StringBuffer();
            ServletInputStream in = request.getInputStream();
            int i = 0;
            while(i<request.getContentLength()) {
                int c = in.read();
                if(c == -1) {
                    break;
                } else {
                    buf.append((char)c);
                }
            }
        } else if("chunked".equalsIgnoreCase(request.getHeader("Transfer-Encoding"))) {
           buf = new StringBuffer();
           ServletInputStream in = request.getInputStream();
           for(int c = in.read(); c != -1; c = in.read()) {
               if(c == -1) {
                   break;
               } else {
                   buf.append((char)c);
               }
           }
        }

        out.println("<html>");
        out.println("<head><title>Request Body Servlet: " + request.getMethod() + "</title></head>");
        out.println("<body>");

        out.println("<p>This is a response to an HTTP " + request.getMethod() + " request.</p>");
        out.println("<p>Body:</p>");
        if(null != buf) {
            out.println("<p><tt>" + buf.toString() + "</tt></p>");
        } else {
            out.println("No body submitted.");
        }
        out.println("</body>");
        out.println("</html>");
    }
}

