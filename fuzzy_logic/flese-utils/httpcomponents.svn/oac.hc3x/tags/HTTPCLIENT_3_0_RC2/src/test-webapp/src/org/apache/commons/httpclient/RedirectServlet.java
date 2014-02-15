/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test-webapp/src/org/apache/commons/httpclient/RedirectServlet.java,v 1.9 2004/02/22 18:08:52 olegk Exp $
 * $Revision: 155418 $
 * $Date: 2005-02-26 14:01:52 +0100 (Sat, 26 Feb 2005) $
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

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class RedirectServlet extends MultiMethodServlet {
    protected void genericService(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        String to = null;
        if(null != request.getParameter("loop")) {
            to = request.getRequestURL().append("?").append(request.getQueryString()).toString();
        } else {
            to = request.getParameter("to");
        }
        if(null == to) {
            to = "/";
        }
        response.setHeader("Location",to);

        try {
            int status = Integer.parseInt(request.getParameter("code"));
            response.setStatus(status);
        } catch (Exception e) {
            response.setStatus(HttpServletResponse.SC_MOVED_TEMPORARILY);
        }

    }
}

