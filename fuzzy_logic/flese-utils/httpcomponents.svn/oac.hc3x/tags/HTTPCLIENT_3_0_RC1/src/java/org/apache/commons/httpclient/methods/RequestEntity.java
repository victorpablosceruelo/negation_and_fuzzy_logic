/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/RequestEntity.java,v 1.4 2004/05/17 21:46:03 olegk Exp $
 * $Revision: 1.4 $
 * $Date: 2004-05-17 23:46:03 +0200 (Mon, 17 May 2004) $
 *
 * ====================================================================
 *
 *  Copyright 2004 The Apache Software Foundation
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

package org.apache.commons.httpclient.methods;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @since 3.0
 */
public interface RequestEntity {

    /**
     * Tests if {@link #writeRequest(OutputStream)} can be called more than once.
     * 
     * @return <tt>true</tt> if the entity can be written to {@link OutputStream} more than once, 
     * <tt>false</tt> otherwise.
     */
    boolean isRepeatable();

    /**
     * Writes the request entity to the given stream.
     * @param out
     * @throws IOException
     */
    void writeRequest(OutputStream out) throws IOException;
    
    /**
     * Gets the request entity's length.
     * @return either a number >= 0 or 
     * {@link org.apache.commons.httpclient.methods.EntityEnclosingMethod#CONTENT_LENGTH_CHUNKED}
     */
    long getContentLength();
    
    /**
     * Gets the entity's content type.  This content type will be used as the value for the
     * "Content-Type" header.
     * @return the entity's content type
     * @see org.apache.commons.httpclient.HttpMethod#setRequestHeader(String, String)
     */
    String getContentType();
    
}
