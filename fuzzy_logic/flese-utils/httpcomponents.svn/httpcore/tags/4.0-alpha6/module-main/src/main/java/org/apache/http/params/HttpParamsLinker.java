/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha6/module-main/src/main/java/org/apache/http/params/HttpParamsLinker.java $
 * $Revision: 542224 $
 * $Date: 2007-05-28 15:34:04 +0200 (Mon, 28 May 2007) $
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

package org.apache.http.params;

import org.apache.http.HttpMessage;
import org.apache.http.params.HttpParams;

/**
 * A utility class that can be used to build parameter hierarchies.
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 542224 $
 */
public class HttpParamsLinker {

    private HttpParamsLinker() {
    }
    
    /**
     * Builds parameter hierarchy by linking parameters associated with 
     * the given HTTP message and the given defaults. 
     * 
     * @param message HTTP message
     * @param defaults default HTTP parameters
     */
    public static void link(final HttpMessage message, final HttpParams defaults) {
        if (message == null) {
            throw new IllegalArgumentException("HTTP message may not be null");
        }
        if (message.getParams() instanceof HttpLinkedParams) {
            ((HttpLinkedParams) message.getParams()).setDefaults(defaults);
        }
    }
    
}
