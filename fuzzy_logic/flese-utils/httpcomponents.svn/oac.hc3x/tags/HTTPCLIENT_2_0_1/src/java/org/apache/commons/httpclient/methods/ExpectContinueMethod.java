/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/ExpectContinueMethod.java,v 1.5.2.3 2004/02/22 18:21:15 olegk Exp $
 * $Revision: 1.5.2.3 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
 * ====================================================================
 *
 *  Copyright 2003-2004 The Apache Software Foundation
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
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * <p>
 * This abstract class serves as a foundation for all HTTP methods 
 * that support 'Expect: 100-continue' handshake.
 * </p>
 * 
 * <p>
 * The purpose of the 100 (Continue) status (refer to section 10.1.1 
 * of the RFC 2616 for more details) is to allow a client that is 
 * sending a request message with a request body to determine if the 
 * origin server is willing to accept the request (based on the request 
 * headers) before the client sends the request body. In some cases,
 * it might either be inappropriate or highly inefficient for the 
 * client to send the body if the server will reject the message 
 * without looking at the body.
 * </p>
 * 
 * <p>
 * 'Expect: 100-continue' handshake should be used with caution,
 * as it may cause problems with HTTP servers and proxies that
 * do not support HTTP/1.1 protocol.
 * </p>
 * 
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 *
 * @since 2.0beta1
 */

public abstract class ExpectContinueMethod extends GetMethod {
    
    /** This flag specifies whether "expect: 100-continue" handshake is
     * to be used prior to sending the request body */
    private boolean useExpectHeader = false;
    
    /** LOG object for this class. */
    private static final Log LOG = LogFactory.getLog(ExpectContinueMethod.class);

    /**
     * No-arg constructor.
     *
     * @since 2.0
     */
    public ExpectContinueMethod() {
        super();
    }

    /**
     * Constructor specifying a URI.
     *
     * @param uri either an absolute or relative URI
     *
     * @since 2.0
     */
    public ExpectContinueMethod(String uri) {
        super(uri);
    }

    /**
     * Constructor specifying a URI and a tempDir.
     *
     * @param uri either an absolute or relative URI
     * @param tempDir directory to store temp files in
     *
     * @deprecated the client is responsible for disk I/O
     * @since 2.0
     */
    public ExpectContinueMethod(String uri, String tempDir) {
        super(uri, tempDir);
    }

    /**
     * Constructor specifying a URI, tempDir and tempFile.
     *
     * @param uri either an absolute or relative URI
     * @param tempDir directory to store temp files in
     * @param tempFile file to store temporary data in
     *
     * @deprecated the client is responsible for disk I/O
     * @since 2.0
     */
    public ExpectContinueMethod(String uri, String tempDir, String tempFile) {
        super(uri, tempDir, tempFile);
    }

    /**
     * <p>
     * Returns <tt>true</tt> if the 'Expect: 100-Continue' handshake
     * is activated. The purpose of the 'Expect: 100-Continue' 
     * handshake to allow a client that is sending a request message 
     * with a request body to determine if the origin server is 
     * willing to accept the request (based on the request headers) 
     * before the client sends the request body.
     * </p>
     * 
     * @return <tt>true</tt> if 'Expect: 100-Continue' handshake is to
     * be used, <tt>false</tt> otherwise.
     * 
     * @since 2.0beta1
     */
    public boolean getUseExpectHeader() {
        return this.useExpectHeader;
    }

    /**
     * <p>
     * Activates 'Expect: 100-Continue' handshake. The purpose of 
     * the 'Expect: 100-Continue' handshake to allow a client that is 
     * sending a request message with a request body to determine if 
     * the origin server is willing to accept the request (based on 
     * the request headers) before the client sends the request body.
     * </p>
     * 
     * <p>
     * The use of the 'Expect: 100-continue' handshake can result in 
     * noticable peformance improvement for entity enclosing requests
     * (such as POST and PUT) that require the target server's 
     * authentication.
     * </p>
     * 
     * <p>
     * 'Expect: 100-continue' handshake should be used with 
     * caution, as it may cause problems with HTTP servers and 
     * proxies that do not support HTTP/1.1 protocol.
     * </p>
     * 
     * @param value boolean value
     * 
     * 
     * @since 2.0beta1
     */
    public void setUseExpectHeader(boolean value) {
        this.useExpectHeader = value;
    }

    /**
     * Returns <tt>true</tt> if there is a request body to be sent.
     * 'Expect: 100-continue' handshake may not be used if request
     * body is not present
     * 
     * @return boolean
     * 
     * @since 2.0beta1
     */
    protected abstract boolean hasRequestContent();

    /**
     * Sets the <tt>Expect</tt> header if it has not already been set, 
     * in addition to the "standard" set of headers.
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
     */
    protected void addRequestHeaders(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        LOG.trace("enter ExpectContinueMethod.addRequestHeaders(HttpState, HttpConnection)");
        
        super.addRequestHeaders(state, conn);
        // If the request is being retried, the header may already be present
        boolean headerPresent = (getRequestHeader("Expect") != null);
        // See if the expect header should be sent
        // = HTTP/1.1 or higher
        // = request body present

        if (getUseExpectHeader() && isHttp11() && hasRequestContent()) {
            if (!headerPresent) {
                setRequestHeader("Expect", "100-continue");
            }
        } else {
            if (headerPresent) {
                removeRequestHeader("Expect");
            }
        }
    }
}
