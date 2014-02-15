/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test-webapp/src/org/apache/commons/httpclient/ParamServlet.java,v 1.4.2.1 2004/02/22 18:21:18 olegk Exp $
 * $Revision: 1.4.2.1 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
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

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
import java.util.*;

public class ParamServlet extends MultiMethodServlet {

    protected void genericService(HttpServletRequest request, HttpServletResponse response) 
        throws IOException, ServletException {

        response.setContentType("text/html");
        PrintWriter out = response.getWriter();
        out.println("<html>");
        out.println("<head><title>Param Servlet: " + request.getMethod() + "</title></head>");
        out.println("<body>");

        out.println("<p>This is a response to an HTTP " + request.getMethod() + " request.</p>");

        out.print("<p>QueryString=");
        if(null == request.getQueryString()) {
            out.print("null");
        } else {
            out.print("\"" + request.getQueryString() + "\"");
        }
        out.println("</p>");

        out.println("<p>Parameters</p>");
        Enumeration e = request.getParameterNames();
        while(e.hasMoreElements()) {
            String name = (String)(e.nextElement());
            String[] values = request.getParameterValues(name);
            if(null == values || values.length < 1) {
                out.println("name=\"" + name + "\";value=null<br>");
            } else {
                for(int i=0;i<values.length;i++) {
                    if(null == values[i]) {
                        out.println("name=\"" + name + "\";value=null<br>");
                    } else {
                        out.println("name=\"" + name + "\";value=\"" + values[i] + "\"<br>");
                    }
                }
            }

        }

        out.println("</body>");
        out.println("</html>");
    }
}

