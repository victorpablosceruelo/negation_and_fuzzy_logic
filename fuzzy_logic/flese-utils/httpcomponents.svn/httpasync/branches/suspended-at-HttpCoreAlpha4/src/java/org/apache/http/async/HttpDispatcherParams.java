/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpasync/branches/suspended-at-HttpCoreAlpha4/src/java/org/apache/http/async/HttpDispatcherParams.java $
 * $Revision: 489367 $
 * $Date: 2006-12-21 15:22:51 +0100 (Thu, 21 Dec 2006) $
 *
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.http.async;


import org.apache.http.params.HttpParams;



/**
 * Provides easy access to parameters for dispatchers.
 * All methods are static. This class can not be instantiated.
 * However, it can be subclassed to add implementation specific parameters.
 * 
 * @author <a href="mailto:rolandw at apache.org">Roland Weber</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 489367 $
 * 
 * @since 1.0
 */
public class HttpDispatcherParams {

    /**
     * Whether to preprocess in an application or background thread.
     * <code>true</code> for background thread,
     * <code>false</code> for application thread.
     */
    public final static String PREPROCESS_BACKGROUND =
        "http.dispatcher.preprocess.background";

    /**
     * The default for {@link #PREPROCESS_BACKGROUND PREPROCESS_BACKGROUND}.
     */
    public final static boolean PREPROCESS_BACKGROUND_DEFAULT = false;


    /**
     * Restricted default constructor.
     * This class can not be instantiated.
     *
     * @throws UnsupportedOperationException    always
     */
    protected HttpDispatcherParams()
        throws UnsupportedOperationException {

        throw new UnsupportedOperationException
            ("This class can not be instantiated.");
    }


    /**
     * Asserts that the <code>params</code> argument is not <code>null</code>.
     *
     * @param params    the parameters to check for existence
     *
     * @throws IllegalArgumentException
     *          iff the <code>params</code> argument is <code>null</code>
     */
    public final static void assertParams(final HttpParams params)
        throws IllegalArgumentException {

        if (params == null)
            throw new IllegalArgumentException
                ("HTTP parameters must not be null.");
    }


    /**
     * Obtains {@link #PREPROCESS_BACKGROUND PREPROCESS_BACKGROUND}.
     *
     * @param params    the parameters from which to obtain
     *
     * @return  the value from the parameters, or the default
     */
    public final static
        boolean getPreprocessBackground(final HttpParams params) {

        assertParams(params);
        return params.getBooleanParameter(PREPROCESS_BACKGROUND,
                                          PREPROCESS_BACKGROUND_DEFAULT);
    }


    /**
     * Specifies {@link #PREPROCESS_BACKGROUND PREPROCESS_BACKGROUND}.
     *
     * @param params    the parameters to update
     * @param value     the value for the parameters
     */
    public final static
        void setPreprocessBackground(HttpParams params, boolean value) {

        assertParams(params);
        params.setBooleanParameter(PREPROCESS_BACKGROUND, value);
    }


} // class HttpDispatcherParams
