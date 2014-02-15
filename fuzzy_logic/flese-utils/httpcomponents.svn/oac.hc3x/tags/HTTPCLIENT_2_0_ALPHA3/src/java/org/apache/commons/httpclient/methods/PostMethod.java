/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/PostMethod.java,v 1.38 2003/02/03 21:21:19 olegk Exp $
 * $Revision: 1.38 $
 * $Date: 2003-02-03 22:21:19 +0100 (Mon, 03 Feb 2003) $
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
import java.io.InputStream;
import java.util.Vector;
import java.util.Iterator;

import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.HttpConnection;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.util.URIUtil;
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
 * @version $Revision: 1.38 $
 * @since 1.0
 */
public class PostMethod extends EntityEnclosingMethod {
    // -------------------------------------------------------------- Constants

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(PostMethod.class);

    /** Custom content encoding. */
    private static final int CUSTOM_CONTENT = 0;
    
    /** URL encoded content. */
    private static final int URL_ENCODED_CONTENT = 1;
    
    /** My content encoding. */
    private int contentEncoding = CUSTOM_CONTENT;

    /** The Content-Type for www-form-urlcoded. */
    public static final String FORM_URL_ENCODED_CONTENT_TYPE = 
        "application/x-www-form-urlencoded";

    /** 
     * The buffered request body consisting of <code>NameValuePair</code>s. 
     * @deprecated Parameters will not be buffered in the future but converted
     * into an InputStream immeadiately.
     */
    private Vector deprecated_parameters = new Vector();

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
     * Set the value of parameter with parameterName to parameterValue. Does
     * not preserve the initial insertion order.
     *
     * @param parameterName name of the parameter
     * @param parameterValue value of the parameter
     *
     * @since 2.0
     * 
     * @deprecated use {@link #setRequestBody(NameValuePair[])}.
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
     * @deprecated use {@link #getRequestBody()} or 
     * {@link #getRequestBodyAsString()}.
     */
    public NameValuePair getParameter(String paramName) {
        LOG.trace("enter PostMethod.getParameter(String)");

        if (paramName == null) {
            return null;
        }

        Iterator iter = deprecated_parameters.iterator();

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
     * @deprecated use {@link #getRequestBody()} or 
     * {@link #getRequestBodyAsString()}.
     */
    public NameValuePair[] getParameters() {
        LOG.trace("enter PostMethod.getParameters()");

        int numPairs = deprecated_parameters.size();
        Object[] objectArr = deprecated_parameters.toArray();
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
     * 
     * @deprecated use {@link #setRequestBody(NameValuePair[])}.
     */
    public void addParameter(String paramName, String paramValue) 
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.addParameter(String, String)");

        if ((paramName == null) || (paramValue == null)) {
            throw new IllegalArgumentException(
                "Arguments to addParameter(String, String) cannot be null");
        } else {
            deprecated_parameters.add(new NameValuePair(paramName, paramValue));
        }
        setRequestBody(getParameters());
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
     * 
     * @deprecated use {@link #setRequestBody(NameValuePair[])}.
     */
    public void addParameter(NameValuePair param) 
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.addParameter(NameValuePair)");

        if (param == null) {
            throw new IllegalArgumentException("NameValuePair may not be null");
        } else {
            addParameter(param.getName(), param.getValue());
        }
    }

    /**
     * Add an Array of parameters to be used in the POST request body. Logs a
     * warning if the parameters argument is null.
     *
     * @param parameters The array of parameters to add.
     *
     * @since 2.0
     * 
     * @deprecated use {@link #setRequestBody(NameValuePair[])}.
     */
    public void addParameters(NameValuePair[] parameters) {
        LOG.trace("enter PostMethod.addParameters(NameValuePair[])");

        if (parameters == null) {
            LOG.warn("Attempt to addParameters(null) ignored");
        } else {
            for (int i = 0; i < parameters.length; i++) {
                addParameter(parameters[i]);
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
     * 
     * @deprecated use {@link #setRequestBody(NameValuePair[])}.
     */
    public boolean removeParameter(String paramName) 
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.removeParameter(String)");

        if (paramName == null) {
            throw new IllegalArgumentException(
                "Argument passed to removeParameter(String) cannot be null");
        }
        boolean removed = true;
        Iterator iter = deprecated_parameters.iterator();

        while (iter.hasNext()) {
            NameValuePair pair = (NameValuePair) iter.next();

            if (paramName.equals(pair.getName())) {
                iter.remove();
                removed = true;
            }
        }
        setRequestBody(getParameters());
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
     * 
     * @deprecated use {@link #setRequestBody(NameValuePair[])}.
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

        Iterator iter = deprecated_parameters.iterator();

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
     * Encode the list of parameters into a query string.
     *
     * @param parameters the list of query name and value
     *
     * @return url encoded form of the parameters
     *
     * @throws IllegalArgumentException if parameters is null
     */
    protected static String generateRequestBody(NameValuePair[] parameters, final String charset) 
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.generateRequestBody(NameValuePair[])");

        if (parameters == null) {
            throw new IllegalArgumentException("Array of parameters may not be null");
        }
        StringBuffer buff = new StringBuffer();

        for (int i = 0; i < parameters.length; i++) {
            if (i > 0) {
                buff.append("&");
            }

            NameValuePair parameter = parameters[i];

            String queryName = null;
            try {
                queryName = URIUtil.encodeWithinQuery(parameter.getName(), charset);
            } catch (URIException e) {
                if (LOG.isWarnEnabled()) {
                    LOG.warn("Encosing error: " + e.toString());
                }
                queryName = parameter.getName();
            }

            buff.append(queryName).append("=");
            String queryValue = null;

            try {
                queryValue = URIUtil.encodeWithinQuery(parameter.getValue(), charset);
            } catch (URIException e) {
                if (LOG.isWarnEnabled()) {
                    LOG.warn("Encosing error: " + e.toString());
                }
                queryValue = parameter.getValue();
            }
            buff.append(queryValue);
        }

        return buff.toString();
    }

    /**
     * Sets the request body to be the specified string.
     * Once this method has been invoked,  the request parameters  cannot be
     * altered until I am {@link #recycle recycled}.
     *
     * @param stringBody Request body content as a string
     *
     * @throws IllegalArgumentException if stringBody is null
     */
    public void setRequestBody(String stringBody)
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.setRequestBody(String)");

        if (stringBody == null) {
            throw new IllegalArgumentException("String body not be null");
        }
        super.setRequestBody(stringBody);
        this.contentEncoding = CUSTOM_CONTENT;
    }

    /**
     * Sets the request body to be the specified inputstream.
     * Once this method has been invoked,  the request parameters  cannot be
     * altered until I am {@link #recycle recycled}.
     *
     * @param streamBody Request body content as {@link java.io.InputStream}
     *
     * @throws IllegalArgumentException if streamBody is null
     */
    public void setRequestBody(InputStream streamBody) 
    throws IllegalArgumentException {
        LOG.trace("enter PostMethod.setRequestBody(InputStream)");

        if (streamBody == null) {
            throw new IllegalArgumentException("Stream body may not be null");
        }
        super.setRequestBody(streamBody);
        this.contentEncoding = CUSTOM_CONTENT;
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
        super.setRequestBody(generateRequestBody(parametersBody, getRequestCharSet()));
        this.contentEncoding = URL_ENCODED_CONTENT;
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

        if (this.contentEncoding == URL_ENCODED_CONTENT) {
            //there are some parameters, so set the contentType header
            if (getRequestHeader("Content-Type") == null) {
                setRequestHeader("Content-Type", FORM_URL_ENCODED_CONTENT_TYPE);
            }
        }
    }

    /**
     * Prepare the method for reuse.
     *
     * @since 1.0
     */
    public void recycle() {
        LOG.trace("enter PostMethod.recycle()");
        this.contentEncoding = CUSTOM_CONTENT;
        this.deprecated_parameters.clear();
        super.recycle();
    }

}
