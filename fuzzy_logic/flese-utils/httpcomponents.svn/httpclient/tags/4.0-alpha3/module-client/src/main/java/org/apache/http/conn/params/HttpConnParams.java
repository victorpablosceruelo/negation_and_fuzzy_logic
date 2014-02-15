/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha3/module-client/src/main/java/org/apache/http/conn/params/HttpConnParams.java $
 * $Revision: 606425 $
 * $Date: 2007-12-22 09:54:55 +0100 (Sat, 22 Dec 2007) $
 *
 * ====================================================================
 *
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
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
 */

package org.apache.http.conn.params;

import org.apache.http.params.HttpParams;

/**
 * An adaptor for accessing HTTP connection parameters in {@link HttpParams}.
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 606425 $
 * 
 * @since 4.0
 */
public class HttpConnParams {

    // since the parameter names have been moved to interfaces,
    // this class currently is a dummy

    /** Disabled default constructor. */
    private HttpConnParams() {
        // no body
    }
}

