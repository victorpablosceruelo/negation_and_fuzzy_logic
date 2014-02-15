/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/main/java/org/apache/http/impl/entity/EntitySerializer.java $
 * $Revision: 472564 $
 * $Date: 2006-11-08 18:59:20 +0100 (Wed, 08 Nov 2006) $
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

package org.apache.http.impl.entity;

import java.io.IOException;
import java.io.OutputStream;

import org.apache.http.HttpEntity;
import org.apache.http.HttpException;
import org.apache.http.HttpMessage;
import org.apache.http.entity.ContentLengthStrategy;
import org.apache.http.io.ChunkedOutputStream;
import org.apache.http.io.ContentLengthOutputStream;
import org.apache.http.io.HttpDataTransmitter;
import org.apache.http.io.IdentityOutputStream;

/**
 * Default implementation of an entity serializer.
 * <p>
 * This entity serializer currently supports only "chunked" and "identitiy" transfer-coding</a>
 * </p>
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 472564 $
 * 
 * @since 4.0
 */
public class EntitySerializer {

    private final ContentLengthStrategy lenStrategy;
    
    public EntitySerializer(final ContentLengthStrategy lenStrategy) {
        super();
        if (lenStrategy == null) {
            throw new IllegalArgumentException("Content length strategy may not be null");
        }
        this.lenStrategy = lenStrategy;
    }

    protected OutputStream doSerialize(
            final HttpDataTransmitter datatransmitter,
            final HttpMessage message) throws HttpException, IOException {
        long len = this.lenStrategy.determineLength(message);
        if (len == ContentLengthStrategy.CHUNKED) {
            return new ChunkedOutputStream(datatransmitter);
        } else if (len == ContentLengthStrategy.IDENTITY) {
            return new IdentityOutputStream(datatransmitter);
        } else {
            return new ContentLengthOutputStream(datatransmitter, len);
        }
    }

    public void serialize(
            final HttpDataTransmitter datatransmitter,
            final HttpMessage message,
            final HttpEntity entity) throws HttpException, IOException {
        if (datatransmitter == null) {
            throw new IllegalArgumentException("HTTP data transmitter may not be null");
        }
        if (message == null) {
            throw new IllegalArgumentException("HTTP message may not be null");
        }
        if (entity == null) {
            throw new IllegalArgumentException("HTTP entity may not be null");
        }
        OutputStream outstream = doSerialize(datatransmitter, message);
        entity.writeTo(outstream);
        outstream.close();
    }
    
}
