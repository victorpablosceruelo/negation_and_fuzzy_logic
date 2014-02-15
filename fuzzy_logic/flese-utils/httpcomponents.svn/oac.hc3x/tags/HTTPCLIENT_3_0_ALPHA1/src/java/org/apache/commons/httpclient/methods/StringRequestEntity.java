/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/StringRequestEntity.java,v 1.2 2004/05/13 02:26:08 mbecke Exp $
 * $Revision: 1.2 $
 * $Date: 2004-05-13 04:26:08 +0200 (Thu, 13 May 2004) $
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
 * [Additional notices, if required by prior licensing conditions]
 *
 */
package org.apache.commons.httpclient.methods;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

import org.apache.commons.httpclient.HeaderElement;
import org.apache.commons.httpclient.NameValuePair;

/**
 * A RequestEntity that contains a String.
 * 
 * @since 3.0
 */
public class StringRequestEntity implements RequestEntity {

    /** The content */
    private String content;
    
    /** The charset */
    private String charset;

    /** The content type (i.e. text/html; charset=EUC-JP). */
    private String contentType;
    

    /**
     * Creates a new entity with the given content
     *  
     * @param content The content to set.
     */
    public StringRequestEntity(String content) {
        this(content, null, null);
    }

    /**
     * Creates a new entity with the given content, content type, and charset.
     *  
     * @param content The content to set.
     * @param contentType The type of the content, or <code>null</code>.  The value retured 
     *   by {@link #getContentType()}.  If this content type contains a charset and the charset
     *   parameter is null, the content's type charset will be used.
     * @param charset The charset of the content, or <code>null</code>.  Used to convert the 
     *   content to bytes.  If the content type does not contain a charset and charset is not null,
     *   then the charset will be appended to the content type.
     */
    public StringRequestEntity(String content, String contentType, String charset) {
        super();
        if (content == null) {
            throw new IllegalArgumentException("The content cannot be null");
        }
        
        this.content = content;
        this.contentType = contentType;
        this.charset = charset;
        
        // resolve the content type and the charset
        if (contentType != null) {
            HeaderElement[] values = HeaderElement.parseElements(contentType);
            NameValuePair charsetPair = null;
            for (int i = 0; i < values.length; i++) {
                if ((charsetPair = values[i].getParameterByName("charset")) != null) {
                    // charset found
                    break;
                }
            }
            if (charset == null && charsetPair != null) {
                // use the charset from the content type
                this.charset = charsetPair.getValue();
            } else if (charset != null && charsetPair == null) {
                // append the charset to the content type
                this.contentType = contentType + "; charset=" + charset; 
            }
        }
    }

    /* (non-Javadoc)
     * @see org.apache.commons.httpclient.methods.RequestEntity#getContentType()
     */
    public String getContentType() {
        return contentType;
    }

    /**
     * @return <code>true</code>
     */
    public boolean isRepeatable() {
        return true;
    }

    /* (non-Javadoc)
     * @see org.apache.commons.httpclient.RequestEntity#writeRequest(java.io.OutputStream)
     */
    public void writeRequest(OutputStream out) throws IOException {
        Writer writer = null;
        if (this.charset != null) {
            writer = new OutputStreamWriter(out, this.charset); 
        } else {
            writer = new OutputStreamWriter(out); 
        }
        writer.write(content);
        writer.flush();
    }

    /**
     * @return The length of the content.
     */
    public long getContentLength() {
        return content.length();
    }

    /**
     * @return Returns the content.
     */
    public String getContent() {
        return this.content;
    }

    /**
     * @return Returns the charset used to convert the content to bytes. <code>null</code> if
     * no charset as been specified.
     */
    public String getCharset() {
        return charset;
    }
}
