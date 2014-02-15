/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/main/java/org/apache/http/entity/ContentLengthStrategy.java $
 * $Revision: 463914 $
 * $Date: 2006-10-14 13:15:28 +0200 (Sat, 14 Oct 2006) $
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

package org.apache.http.entity;

import org.apache.http.HttpException;
import org.apache.http.HttpMessage;

/**
 * Represents a strategy to determine the content length based on the properties
 * of an HTTP message.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 463914 $
 * 
 * @since 4.0
 */
public interface ContentLengthStrategy {

    public static final int IDENTITY         = -1;
    public static final int CHUNKED          = -2;
    
    long determineLength(HttpMessage message) throws HttpException;
            
}
