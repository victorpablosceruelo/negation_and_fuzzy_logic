/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/HeadMethod.java,v 1.19.2.4 2004/06/13 20:24:49 olegk Exp $
 * $Revision: 1.19.2.4 $
 * $Date: 2004-06-13 22:24:49 +0200 (Sun, 13 Jun 2004) $
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

package org.apache.commons.httpclient.methods;

import java.io.IOException;

import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethodBase;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Implements the HTTP HEAD method.
 * <p>
 * The HTTP HEAD method is defined in section 9.4 of 
 * <a href="http://www.ietf.org/rfc/rfc2616.txt">RFC2616</a>:
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
 * @author <a href="mailto:oleg@ural.ru">oleg Kalnichevski</a>
 *
 * @version $Revision: 1.19.2.4 $
 * @since 1.0
 */
public class HeadMethod extends HttpMethodBase {
    //~ Static variables/initializers ··········································

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(HeadMethod.class);

    private int bodyCheckTimeout = -1; /* Disabled per default */

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
     * Recycles the HTTP method so that it can be used again.
     * Note that all of the instance variables will be reset
     * once this method has been called. This method will also
     * release the connection being used by this HTTP method.
     * 
     * @see #releaseConnection()
     * 
     * @since 1.0
     * 
     * @deprecated no longer supported and will be removed in the future
     *             version of HttpClient
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
     * @param state the {@link HttpState state} information associated with this method
     * @param conn the {@link HttpConnection connection} used to execute
     *        this HTTP method
     *
     * @throws IOException if an I/O (transport) error occurs
     * @throws HttpException  if a protocol exception occurs.
     * @throws HttpRecoverableException if a recoverable transport error occurs. 
     *                    Usually this kind of exceptions can be recovered from by
     *                    retrying the HTTP method 
     *
     * @see #readResponse
     * @see #processResponseBody
     * 
     * @since 2.0
     */
    protected void readResponseBody(HttpState state, HttpConnection conn)
    throws IOException {
        LOG.trace(
            "enter HeadMethod.readResponseBody(HttpState, HttpConnection)");
        
        if (this.bodyCheckTimeout < 0) {
            responseBodyConsumed();
        } else {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Check for non-compliant response body. Timeout in " 
                 + this.bodyCheckTimeout + " ms");    
            }
            boolean responseAvailable = false;
            try {
                responseAvailable = conn.isResponseAvailable(this.bodyCheckTimeout);
            } catch (IOException e) {
                LOG.debug("An IOException occurred while testing if a response was available,"
                    + " we will assume one is not.", 
                    e);
                responseAvailable = false;
            }
            if (responseAvailable) {
                if (isStrictMode()) {
                    throw new HttpException(
                        "Body content may not be sent in response to HTTP HEAD request");
                } else {
                    LOG.warn("Body content returned in response to HTTP HEAD");    
                }
                super.readResponseBody(state, conn);
            }
        }

    }
    
    /**
     * Return non-compliant response body check timeout.
     * 
     * @return The period of time in milliseconds to wait for a response 
     *         body from a non-compliant server. <tt>-1</tt> returned when 
     *         non-compliant response body check is disabled
     */
    public int getBodyCheckTimeout() {
        return this.bodyCheckTimeout;
    }

    /**
     * Set non-compliant response body check timeout.
     * 
     * @param timeout The period of time in milliseconds to wait for a response 
     *         body from a non-compliant server. <tt>-1</tt> can be used to 
     *         disable non-compliant response body check 
     */
    public void setBodyCheckTimeout(int timeout) {
        this.bodyCheckTimeout = timeout;
    }

}
