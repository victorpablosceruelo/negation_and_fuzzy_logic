/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha3/module-httpmime/src/main/java/org/apache/http/entity/mime/RFC822Header.java $
 * $Revision: 618333 $
 * $Date: 2008-02-04 18:27:24 +0100 (Mon, 04 Feb 2008) $
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

package org.apache.http.entity.mime;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Iterator;

import org.apache.james.mime4j.message.Header;

/**
 * {@link Header} implementation with the stricter RDC 822 compliance.
 * To be removed if resolved in mime4j.
 */
class RFC822Header extends Header {

    @Override
    public void writeTo(final OutputStream out) throws IOException {
        BufferedWriter writer = new BufferedWriter(
                new OutputStreamWriter(out, MIME.DEFAULT_CHARSET), 8192);
        for (Iterator<?> it = getFields().iterator(); it.hasNext();) {
            writer.write(it.next().toString());
            writer.write("\r\n");
        }
        writer.write("\r\n");
        writer.flush();
    }

}
