/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/main/java/org/apache/http/HttpResponseFactory.java $
 * $Revision: 378645 $
 * $Date: 2006-02-17 23:34:54 +0100 (Fri, 17 Feb 2006) $
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

package org.apache.http;

/**
 * A factory for {@link HttpResponse HttpResponse} objects.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 378645 $
 * 
 * @since 4.0
 */
public interface HttpResponseFactory {
    
    HttpResponse newHttpResponse(HttpVersion ver, int status);
    
    HttpResponse newHttpResponse(StatusLine statusline);
    
}
