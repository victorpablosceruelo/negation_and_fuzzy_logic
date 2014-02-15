/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/examples/CookieDemoApp.java,v 1.6 2003/01/23 22:47:42 jsdever Exp $
 * $Revision: 1.6 $
 * $Date: 2003-01-23 23:48:49 +0100 (Thu, 23 Jan 2003) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "HttpClient", and "Apache Software
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

import org.apache.commons.httpclient.*;
import org.apache.commons.httpclient.methods.*;
import java.io.*;
import java.net.URL;

/**
 * 
 * This is a sample application that demonstrates
 * how to use the Jakarta HttpClient API. 
 * 
 * This application sets an HTTP cookie and 
 * updates the cookie's value across multiple
 * HTTP GET requests.
 * 
 * @author Sean C. Sullivan
 *
 */
public class CookieDemoApp {
	private static final String COOKIE_NAME = "count";

    /**
     *  
     * Usage:
     *          java CookieDemoApp http://mywebserver:80/
     * 
     *  @param args command line arguments
     *                 Argument 0 is a URL to a web server
     *                
     * 
     */
    public static void main(String[] args) throws Exception {
        if (args.length != 1) {
            System.err.println("Usage: java CookieDemoApp <url>");
            System.err.println("<url> The url of a webpage");
			System.exit(1);
		}
		
		String strURL = args[0];
		
		URL u = new URL(strURL);
		
		HttpState initialState = new HttpState();

		Cookie ck = new Cookie(".foobar.com",
				COOKIE_NAME,
				"0");

		initialState.addCookie(ck);

        HttpClient client = new HttpClient();
        HostConfiguration hc = new HostConfiguration();
        hc.setHost(new URI(u));
        client.setHostConfiguration(hc);
		
        client.setConnectionTimeout(30000);
        client.setState(initialState);

        for (int i = 0; i < 10; i++) {
			GetMethod get = new GetMethod("/");
            int iResultCode = client.executeMethod(get);
            HttpState state = client.getState();
			Cookie[] cookies = state.getCookies();
            for (int k = 0; k < cookies.length; k++) {
				Cookie currentCookie = cookies[k];
                if (currentCookie.getName().equals(COOKIE_NAME)) {
					Integer iCount = new Integer(currentCookie.getValue());
					System.out.println("count value is : " + iCount);
					int iNewCount = iCount.intValue() + 1;
					currentCookie.setValue("" + iNewCount);
				}
			}
            get.releaseConnection();
		}
	}
}
