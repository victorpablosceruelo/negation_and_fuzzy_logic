/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha4/module-nio/src/main/java/org/apache/http/nio/protocol/BufferedContent.java $
 * $Revision: 508428 $
 * $Date: 2007-02-16 15:52:10 +0100 (Fri, 16 Feb 2007) $
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

package org.apache.http.nio.protocol;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.entity.BasicHttpEntity;
import org.apache.http.nio.util.ContentInputBuffer;

class BufferedContent extends BasicHttpEntity {

    /** The wrapped entity. */
    private HttpEntity wrappedEntity;
    
    public BufferedContent(final HttpEntity entity, final ContentInputBuffer buffer) {
        super();
        if (entity == null) {
            throw new IllegalArgumentException("HTTP entity may not be null");
        }
        this.wrappedEntity = entity;
        setContent(new ContentInputStream(buffer));
    }

    public boolean isChunked() {
        return this.wrappedEntity.isChunked();
    }

    public long getContentLength() {
        return this.wrappedEntity.getContentLength();
    }

    public Header getContentType() {
        return this.wrappedEntity.getContentType();
    }

    public Header getContentEncoding() {
        return this.wrappedEntity.getContentEncoding();
    }
    
}
