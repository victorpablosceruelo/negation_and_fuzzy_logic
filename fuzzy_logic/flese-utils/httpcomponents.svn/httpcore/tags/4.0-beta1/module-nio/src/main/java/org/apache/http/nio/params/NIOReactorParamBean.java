/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-beta1/module-nio/src/main/java/org/apache/http/nio/params/NIOReactorParamBean.java $
 * $Revision: 593937 $
 * $Date: 2007-11-11 19:44:12 +0100 (Sun, 11 Nov 2007) $
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

package org.apache.http.nio.params;

import org.apache.http.params.HttpAbstractParamBean;
import org.apache.http.params.HttpParams;

public class NIOReactorParamBean extends HttpAbstractParamBean {
    
    public NIOReactorParamBean (final HttpParams params) {
        super(params);
    }

    public void setContentBufferSize (int contentBufferSize) {
        NIOReactorParams.setContentBufferSize(params, contentBufferSize);
    }

    public void setSelectInterval (long selectInterval) {
        NIOReactorParams.setSelectInterval(params, selectInterval);
    }
    
}
