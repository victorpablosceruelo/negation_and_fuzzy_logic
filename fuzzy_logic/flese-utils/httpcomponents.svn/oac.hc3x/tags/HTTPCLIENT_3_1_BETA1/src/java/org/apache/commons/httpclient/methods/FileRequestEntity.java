/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/oac.hc3x/tags/HTTPCLIENT_3_1_BETA1/src/java/org/apache/commons/httpclient/methods/FileRequestEntity.java $
 * $Revision: 410820 $
 * $Date: 2006-06-01 12:01:33 +0200 (Thu, 01 Jun 2006) $
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
package org.apache.commons.httpclient.methods;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.httpclient.methods.RequestEntity;

/**
 * A RequestEntity that represents a File.
 * 
 * @since 3.1
 */
public class FileRequestEntity implements RequestEntity {

    final File file;
    final String contentType;
    
    public FileRequestEntity(final File file, final String contentType) {
        super();
        if (file == null) {
            throw new IllegalArgumentException("File may not be null");
        }
        this.file = file;
        this.contentType = contentType;
    }
    public long getContentLength() {
        return this.file.length();
    }

    public String getContentType() {
        return this.contentType;
    }

    public boolean isRepeatable() {
        return true;
    }

    public void writeRequest(final OutputStream out) throws IOException {
        byte[] tmp = new byte[4096];
        int i = 0;
        InputStream instream = new FileInputStream(this.file);
        while ((i = instream.read(tmp)) >= 0) {
            out.write(tmp, 0, i);
        }        
    }    
    
}
