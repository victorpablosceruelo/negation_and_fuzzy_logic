/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/HeadMethod.java,v 1.17 2003/02/02 04:30:13 jsdever Exp $
 * $Revision: 1.17 $
 * $Date: 2003-02-02 05:30:13 +0100 (Sun, 02 Feb 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
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

package org.apache.commons.httpclient.methods;

import java.io.IOException;

import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpMethodBase;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Implements the HTTP HEAD specification.
 * <p>
 * The HTTP HEAD method is defined in section 8.2 of 
 * <a href="http://www.ietf.org/rfc/rfc1945.txt">RFC1945</a>:
 * <blockquote>
 * The HEAD method is identical to GET except that the server MUST NOT
 * return a message-body in the response. The metainformation contained
 * in the HTTP headers in response to a HEAD request SHOULD be identical
 * to the information sent in response to a GET request. This method can
 * be used for obtaining metainformation about the entity implied by the
 * request without transferring the entity-body itself. This method is
 * often used for testing hypertext links for validity, accessibility,
 * and recent modification.
 * </blockquote>
 * </p>
 * 
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 *
 * @version $Revision: 1.17 $
 * @since 1.0
 */
public class HeadMethod extends HttpMethodBase {
    //~ Static variables/initializers ··········································

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(HeadMethod.class);

    //~ Constructors ···························································

    /**
     * No-arg constructor.
     * 
     * @since 1.0
     */
    public HeadMethod() {
        setFollowRedirects(true);
    }

    /**
     * Constructor specifying a URI.
     *
     * @param uri either an absolute or relative URI
     * 
     * @since 1.0
     */
    public HeadMethod(String uri) {
        super(uri);
        setFollowRedirects(true);
    }

    //~ Methods ································································

    /**
     * Returns <tt>"HEAD"</tt>.
     * 
     * @return <tt>"HEAD"</tt>
     * 
     * @since 2.0
     */
    public String getName() {
        return "HEAD";
    }

    /**
     * Override recycle to reset redirects default.
     * 
     * @since 1.0
     */
    public void recycle() {
        super.recycle();
        setFollowRedirects(true);
    }

    /**
     * Overrides {@link HttpMethodBase} method to <i>not</i> read a response
     * body, despite the presence of a <tt>Content-Length</tt> or
     * <tt>Transfer-Encoding</tt> header.
     * 
     * @param state shared http state
     * @param conn the http connection to read from
     * 
     * @throws IOException when there's an error reading data
     * 
     * @since 2.0
     */
    protected void readResponseBody(HttpState state, HttpConnection conn)
    throws IOException {
        LOG.trace(
            "enter HeadMethod.readResponseBody(HttpState, HttpConnection)");

        // despite the possible presence of a content-length header,
        // HEAD returns no response body
        responseBodyConsumed();
        return;
    }
}
