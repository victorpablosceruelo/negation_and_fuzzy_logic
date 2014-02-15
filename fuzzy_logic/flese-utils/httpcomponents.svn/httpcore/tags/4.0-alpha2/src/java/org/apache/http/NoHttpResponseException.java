/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha2/src/java/org/apache/http/NoHttpResponseException.java $
 * $Revision: 376458 $
 * $Date: 2006-02-09 23:22:06 +0100 (Thu, 09 Feb 2006) $
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

import java.io.IOException;

/**
 * <p>
 * Signals that the target server failed to respond with a valid HTTP response.
 * </p>
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 376458 $
 */
public class NoHttpResponseException extends IOException {

	static final long serialVersionUID = -7658940387386078766L;
	
    /**
     * Creates a new NoHttpResponseException with the specified detail message.
     *
     * @param message exception message
     */
    public NoHttpResponseException(String message) {
        super(message);
    }

}
