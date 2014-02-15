/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/main/java/org/apache/http/cookie/params/CookieSpecParams.java $
 * $Revision: 535132 $
 * $Date: 2007-05-04 11:01:16 +0200 (Fri, 04 May 2007) $
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

package org.apache.http.cookie.params;

import org.apache.http.params.HttpParams;

/**
 * This class implements an adaptor around the {@link HttpParams} interface
 * to simplify manipulation of cookie management specific parameters.
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 535132 $
 * 
 * @since 4.0
 */
public final class CookieSpecParams {

    /**
     * The key used to look up the date patterns used for parsing. The String patterns are stored
     * in a {@link java.util.Collection} and must be compatible with 
     * {@link java.text.SimpleDateFormat}.
     * <p>
     * This parameter expects a value of type {@link java.util.Collection}.
     * </p>
     */
    public static final String DATE_PATTERNS = "http.protocol.cookie-datepatterns";
    
    /**
     * Defines whether {@link org.apache.http.cookie.Cookie cookies} should be put on 
     * a single {@link org.apache.http.Header request header}.
     * <p>
     * This parameter expects a value of type {@link Boolean}.
     * </p>
     */
    public static final String SINGLE_COOKIE_HEADER = "http.protocol.single-cookie-header"; 

}
