/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/PostMethod.java,v 1.45 2003/06/23 23:41:40 mbecke Exp $
 * $Revision: 1.45 $
 * $Date: 2003-06-24 01:41:40 +0200 (Tue, 24 Jun 2003) $
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
import java.util.Iterator;
import java.util.Vector;

import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpConstants;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Implements the HTTP POST specification.
 * <p>
 * The HTTP POST method is defined in section 8.3 of
 * <a href="http://www.ietf.org/rfc/rfc1945.txt">RFC1945</a>:
 * <blockquote>
 * The POST method is used to request that the origin server accept the entity
 * enclosed in the request as a new subordinate of the resource identified by
 * the Request-URI in the Request-Line. POST is designed to allow a uniform
 * method to cover the following functions:
 * <ul>
 *   <li>Annotation of existing resources</li>
 *   <li>Posting a message to a bulletin board, newsgroup, mailing list, or 
 *     similar group of articles</li>
 *   <li>Providing a block of data, such as the result of submitting a form,
 *     to a data-handling process</li>
 *   <li>Extending a database through an append operation</li>
 * </ul>
 * </blockquote>
 * </p>
 *
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author <a href="mailto:dsale@us.britannica.com">Doug Sale</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author Ortwin Glück
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 1.45 $
 * @since 1.0
 */
public class PostMethod extends EntityEnclosingMethod {
    // -------------------------------------------------------------- Constants

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(PostMethod.class);

    /** The Content-Type for www-form-urlencoded. */
    public static final String FORM_URL_ENCODED_CONTENT_TYPE = 
        "application/x-www-form-urlencoded";

    /** 
     * The buffered request body consisting of <code>NameValuePair</code>s. 
     */
    private Vector params = new Vector();

    // ----------------------------------------------------------- Constructors

    /**
     * No-arg constructor.
     *
     * @since 1.0
     */
    public PostMethod() {
        super();
    }

    /**
     * Constructor specifying a URI.
     *
     * @param uri either an absolute or relative URI
     *
     * @since 1.0
     */
    public PostMethod(String uri) {
        super(uri);
    }

    /**
     * Constructor specifying a URI and a tempDir.
     *
     * @param uri either an absolute or relative URI
     * @param tempDir directory to store temp files in
     *
     * @deprecated the client is responsible for disk I/O
     * @since 1.0
     */
    public PostMethod(String uri, String tempDir) {
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
     * @since 1.0
     */
    public PostMethod(String uri, String tempDir, String tempFile) {
        super(uri, tempDir, tempFile);
    }

    // ----------------------------------------------------- Instance Methods

    /**
     * Returns <tt>"POST"</tt>.
     *
     * @return <tt>"POST"</tt>
     *
     * @since 2.0
     */
    public String getName() {
        return "POST";
    }


    /**
     * Returns <tt>true</tt> if there is a request body to be sent.
     * 
     * <P>This method must be overwritten by sub-classes that implement
     * alternative request content input methods
     * </p>
     * 
     * @return boolean
     * 
     * @since 2.0beta1
     */
    protected boolean hasRequestContent() {
        LOG.trace("enter PostMethod.hasRequestContent()");
        if (!this.params.isEmpty()) {
            return true;
        } else {
            return super.hasRequestContent();
        }
    }

    /**
     * Clears request body.
     * 
     * <p>This method must be overwritten by sub-classes that implement
     * alternative request content input methods</p>
     * 
     * @since 2.0beta1
     */
    protected void clearRequestBody() {
        LOG.trace("enter PostMethod.clearRequestBody()");
        this.params.clear();
        super.clearRequestBody();
    }

    /**
     * Generates request body.
     * 
     * <p>This method must be overwritten by sub-classes that implement
     * alternative request content input methods
     * </p>   
     * 
     * @return request body as an array of bytes. If the request content 
     *          has not been set, returns <tt>null</null>.
     * 
     * @since 2.0beta1
     */
    protected byte[] generateRequestBody() {
        LOG.trace("enter PostMethod.renerateRequestBody()");
        if (!this.params.isEmpty()) {
            String content = formUrlEncode(getParameters(), getRequestCharSet());
            return HttpConstants.getContentBytes(content);
        } else {
            return super.generateRequestBody();
        }
    }


    /**
     * Set the value of parameter with parameterName to parameterValue. Does
     * not preserve the initial insertion order.
     *
     * @param parameterName name of the parameter
     * @param parameterValue value of the parameter
     *
     * @since 2.0
     */
    public void setParameter(String parameterName, String parameterValue) {
        LOG.trace("enter PostMethod.setParameter(String, String)");

        removeParameter(parameterName, parameterValue);
        addParameter(parameterName, parameterValue);
    }

    /**
     * Gets the parameter of the specified name. If there exists more than one
     * parameter with the name paramName, then only the first one is returned.
     *
     * @param paramName name of the parameter
     *
     * @return If a parameter exists with the name argument, the coresponding
     *         NameValuePair is returned.  Otherwise null.
     *
     * @since 2.0
     * 
     */
    public NameValuePair getParameter(String paramName) {
        LOG.trace("enter PostMethod.getParameter(String)");

        if (paramName == null) {
            return null;
        }

        Iterator iter = this.params.iterator();

        while (iter.hasNext()) {
            NameValuePair parameter = (NameValuePair) iter.next();

            if (paramName.equals(parameter.getName())) {
                return parameter;
            }
        }
        return null;
    }

    /**
     * Gets the parameters currently added to the PostMethod. If there are no
     * parameters, a valid array is returned with zero elements. The returned
     * array object contains an array of pointers to  the internal data
     * members.
     *
     * @return An array of the current parameters
     *
     * @since 2.0
     * 
     */
    public NameValuePair[] getParameters() {
        LOG.trace("enter PostMethod.getParameters()");

        int numPairs = this.params.size();
        Object[] objectArr = this.params.toArray();
        NameValuePair[] nvPairArr = new NameValuePair[numPairs];

        for (int i = 0; i < numPairs; i++) {
            nvPairArr[i] = (NameValuePair) objectArr[i];
        }

        return nvPairArr;
    }

    /**
     * Add a new parameter to be used in the POST request body.
     *
     * @param paramName The parameter name to add.
     * @param paramValue The parameter value to add.
     *
     * @throws IllegalArgumentException if either argument is null
     *
     * @since 1.0
     */
    public void addParameter(String paramName, String paramValue) 
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.addParameter(String, String)");

        if ((paramName == null) || (paramValue == null)) {
            throw new IllegalArgumentException(
                "Arguments to addParameter(String, String) cannot be null");
        }
        super.clearRequestBody();
        this.params.add(new NameValuePair(paramName, paramValue));
    }

    /**
     * Add a new parameter to be used in the POST request body.
     *
     * @param param The parameter to add.
     *
     * @throws IllegalArgumentException if the argument is null or contains
     *         null values
     *
     * @since 2.0
     */
    public void addParameter(NameValuePair param) 
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.addParameter(NameValuePair)");

        if (param == null) {
            throw new IllegalArgumentException("NameValuePair may not be null");
        }
        addParameter(param.getName(), param.getValue());
    }

    /**
     * Add an Array of parameters to be used in the POST request body. Logs a
     * warning if the parameters argument is null.
     *
     * @param parameters The array of parameters to add.
     *
     * @since 2.0
     */
    public void addParameters(NameValuePair[] parameters) {
        LOG.trace("enter PostMethod.addParameters(NameValuePair[])");

        if (parameters == null) {
            LOG.warn("Attempt to addParameters(null) ignored");
        } else {
            super.clearRequestBody();
            for (int i = 0; i < parameters.length; i++) {
                this.params.add(parameters[i]);
            }
        }
    }

    /**
     * Removes all parameters with the given paramName. If there is more than
     * one parameter with the given paramName, all of them are removed.  If
     * there is just one, it is removed.  If there are none, then the request
     * is ignored.
     *
     * @param paramName The parameter name to remove.
     *
     * @return true if at least one parameter was removed
     *
     * @throws IllegalArgumentException When the parameter name passed is null
     *
     * @since 2.0
     */
    public boolean removeParameter(String paramName) 
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.removeParameter(String)");

        if (paramName == null) {
            throw new IllegalArgumentException(
                "Argument passed to removeParameter(String) cannot be null");
        }
        boolean removed = true;
        Iterator iter = this.params.iterator();

        while (iter.hasNext()) {
            NameValuePair pair = (NameValuePair) iter.next();

            if (paramName.equals(pair.getName())) {
                iter.remove();
                removed = true;
            }
        }
        return removed;
    }

    /**
     * Removes all parameter with the given paramName and paramValue. If there
     * is more than one parameter with the given paramName, only one is
     * removed.  If there are none, then the request is ignored.
     *
     * @param paramName The parameter name to remove.
     * @param paramValue The parameter value to remove.
     *
     * @return true if a parameter was removed.
     *
     * @throws IllegalArgumentException when param name or value are null
     *
     * @since 2.0
     */
    public boolean removeParameter(String paramName, String paramValue) 
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.removeParameter(String, String)");

        if (paramName == null) {
            throw new IllegalArgumentException("Parameter name may not be null");
        }
        if (paramValue == null) {
            throw new IllegalArgumentException("Parameter value may not be null");
        }

        Iterator iter = this.params.iterator();

        while (iter.hasNext()) {
            NameValuePair pair = (NameValuePair) iter.next();

            if (paramName.equals(pair.getName())
                && paramValue.equals(pair.getValue())) {
                iter.remove();
                return true;
            }
        }

        return false;
    }

    /**
     * Set an Array of parameters to be used in the POST request body
     *
     * @param parametersBody The array of parameters to add.
     *
     * @throws IllegalArgumentException when param parameters are null
     * 
     * @since 2.0beta1
     */
    public void setRequestBody(NameValuePair[] parametersBody)
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.setRequestBody(NameValuePair[])");

        if (parametersBody == null) {
            throw new IllegalArgumentException("Array of parameters may not be null");
        }
        clearRequestBody();
        addParameters(parametersBody);
    }

    /**
     * Override method of {@link org.apache.commons.httpclient.HttpMethodBase}
     * to  also add <tt>Content-Type</tt> header when appropriate.
     *
     * @param state the client state
     * @param conn the {@link HttpConnection} the headers will eventually be
     *        written to
     * @throws IOException when an error occurs writing the request
     * @throws HttpException when a HTTP protocol error occurs
     *
     * @since 2.0
     */
    protected void addRequestHeaders(HttpState state, HttpConnection conn)
    throws IOException, HttpException {
        super.addRequestHeaders(state, conn);

        if (!this.params.isEmpty()) {
            //there are some parameters, so set the contentType header
            if (getRequestHeader("Content-Type") == null) {
                setRequestHeader("Content-Type", FORM_URL_ENCODED_CONTENT_TYPE);
            }
        }
    }

}
