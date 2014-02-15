/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/main/java/org/apache/http/MalformedChunkCodingException.java $
 * $Revision: 473982 $
 * $Date: 2006-11-12 17:17:43 +0100 (Sun, 12 Nov 2006) $
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
 * Signals a malformed chunked stream.
 */
public class MalformedChunkCodingException extends IOException {

	static final long serialVersionUID = 3138679343859749668L;
	
    /**
     * Creates a MalformedChunkCodingException with a <tt>null</tt> detail message.
     */
    public MalformedChunkCodingException() {
        super();
    }

    /**
     * Creates a MalformedChunkCodingException with the specified detail message.
     * 
     * @param message The exception detail message 
     */
    public MalformedChunkCodingException(final String message) {
        super(message);
    }

}
