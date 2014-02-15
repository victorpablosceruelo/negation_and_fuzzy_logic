/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-main/src/main/java/org/apache/http/message/BasicHeader.java $
 * $Revision: 503412 $
 * $Date: 2007-02-04 15:14:42 +0100 (Sun, 04 Feb 2007) $
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

package org.apache.http.message;

import org.apache.http.Header;
import org.apache.http.HeaderElement;
import org.apache.http.util.CharArrayBuffer;

/**
 * Represents an HTTP header field.
 * 
 * <p>The HTTP header fields follow the same generic format as
 * that given in Section 3.1 of RFC 822. Each header field consists
 * of a name followed by a colon (":") and the field value. Field names
 * are case-insensitive. The field value MAY be preceded by any amount
 * of LWS, though a single SP is preferred. 
 *
 *<pre>
 *     message-header = field-name ":" [ field-value ]
 *     field-name     = token
 *     field-value    = *( field-content | LWS )
 *     field-content  = &lt;the OCTETs making up the field-value
 *                      and consisting of either *TEXT or combinations
 *                      of token, separators, and quoted-string&gt;
 *</pre>
 * 
 * @author <a href="mailto:remm@apache.org">Remy Maucherat</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 *
 * <!-- empty lines above to avoid 'svn diff' context problems -->
 * @version $Revision: 503412 $ $Date: 2007-02-04 15:14:42 +0100 (Sun, 04 Feb 2007) $
 * 
 * @since 4.0
 */
public class BasicHeader implements Header {

    /**
     * Header name.
     */
    private final String name;
    
    /**
     * Header value.
     */
    private final String value;
    
    /**
     * Constructor with name and value
     *
     * @param name the header name
     * @param value the header value
     */
    public BasicHeader(final String name, final String value) {
        super();
        if (name == null) {
            throw new IllegalArgumentException("Name may not be null");
        }
        this.name = name;
        this.value = value;
    }

    /**
     * Returns the header name.
     *
     * @return String name The name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Returns the header value.
     *
     * @return String value The current value.
     */
    public String getValue() {
        return this.value;
    }

    /**
     * Returns a {@link String} representation of the header.
     *
     * @return a string
     */
    public String toString() {
        CharArrayBuffer buffer = new CharArrayBuffer(32);
        buffer.append(this.name);
        buffer.append(": ");
        if (this.value != null) {
            buffer.append(this.value);
        }
        return buffer.toString();
    }

    /**
     * Returns an array of {@link HeaderElement}s constructed from my value.
     *
     * @see BasicHeaderElement#parseAll
     * 
     * @return an array of header elements
     */
    public HeaderElement[] getElements() {
        if (this.value != null) {
            return BasicHeaderElement.parseAll(this.value);
        } else {
            return new HeaderElement[] {}; 
        }
    }

    /**
     * Formats a Header into a header line. The <code>header</code> is
     * directly appended to <code>buffer</code>; no newline characters are
     * inserted (folding).
     * 
     * @param buffer the buffer to append to
     * @param header the header to format
     */
    public static void format(final CharArrayBuffer buffer, final Header header) {
        if (buffer == null) {
            throw new IllegalArgumentException("String buffer may not be null");
        }
        if (header == null) {
            throw new IllegalArgumentException("Header may not be null");
        }
        buffer.append(header.getName());
        buffer.append(": ");
        if (header.getValue() != null) {
            buffer.append(header.getValue());
        }
    }
 
    /**
     * @see #format(CharArrayBuffer, Header)
     */
    public static String format(final Header header) {
        CharArrayBuffer buffer = new CharArrayBuffer(32);
        format(buffer, header);
        return buffer.toString();
    }

}
