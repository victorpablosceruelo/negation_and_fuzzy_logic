/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/examples/PostXML.java,v 1.6 2003/01/23 22:47:42 jsdever Exp $
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
 * This application sends an XML document 
 * to a remote web server using HTTP POST
 * 
 * @author Sean C. Sullivan
 * @author Ortwin Glück
 */
public class PostXML {

    /**
     *  
     * Usage:
     *          java PostXML http://mywebserver:80/ c:\foo.xml
     * 
     *  @param args command line arguments
     *                 Argument 0 is a URL to a web server
     *                 Argument 1 is a local filename
     * 
     */
    public static void main(String[] args) throws Exception {
        if (args.length != 2) {
            System.out.println("Usage: java -classpath <classpath> [-Dorg.apache.commons.logging.simplelog.defaultlog=<loglevel>] PostXML <url> <filename>]");
            System.out.println("<classpath> - must contain the commons-httpclient.jar and commons-logging.jar");
            System.out.println("<loglevel> - one of error, warn, info, debug, trace");
            System.out.println("<url> - the URL to post the file to");
            System.out.println("<filename> - file to post to the URL");
            System.out.println();
			System.exit(1);
		}
		
		String strURL = args[0];
		
		URL u = new URL(strURL);
		
		String strXMLFilename = args[1];

		InputStream input = null;
		
		input = new FileInputStream(strXMLFilename);

		PostMethod post = new PostMethod();

		post.setRequestBody(input);
		
        if ((u.getPath() == null) || (u.getPath().length() == 0)) {
			post.setPath("/");
		}
        else {
			post.setPath(u.getPath());
		}
		post.setRequestHeader("Content-type", "text/xml");
				
		HttpClient hc = new HttpClient();
        HostConfiguration cfg = new HostConfiguration();
        cfg.setHost(new URI(u));
		
		int iResultCode = hc.executeMethod(post);
		
		System.out.println("iResultCode = " + iResultCode);

		System.out.println("Server response:" + post.getResponseBodyAsString() );

        post.releaseConnection();
	}
}
