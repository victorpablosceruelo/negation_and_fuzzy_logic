/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/test/java/org/apache/http/mockup/HttpMessageMockup.java $
 * $Revision: 385851 $
 * $Date: 2006-03-14 19:46:52 +0100 (Tue, 14 Mar 2006) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2006 The Apache Software Foundation
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
 */

package org.apache.http.mockup;

import org.apache.http.HttpVersion;
import org.apache.http.message.AbstractHttpMessage;
import org.apache.http.params.HttpProtocolParams;

/**
 *  {@link HttpMessage} mockup implementation.
 *  
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public class HttpMessageMockup extends AbstractHttpMessage {
    
    public HttpMessageMockup() {
        super();
    }

    public HttpVersion getHttpVersion() {
        return HttpProtocolParams.getVersion(this.getParams());
    }
    
}
