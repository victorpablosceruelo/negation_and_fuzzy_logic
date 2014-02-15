/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha1/src/java/org/apache/http/Header.java $
 * $Revision: 391251 $
 * $Date: 2006-04-04 10:52:13 +0200 (Tue, 04 Apr 2006) $
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
import java.util.ArrayList;

import org.apache.http.io.CharArrayBuffer;
import org.apache.http.io.HttpDataReceiver;

/**
 * <p>Represents an HTTP header field.</p>
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
 * @version $Revision: 391251 $ $Date: 2006-04-04 10:52:13 +0200 (Tue, 04 Apr 2006) $
 */
public class Header {

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
    public Header(final String name, final String value) {
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
     * @see HeaderElement#parseAll
     * 
     * @return an array of header elements
     * 
     * @since 3.0
     */
    public HeaderElement[] getElements() {
        if (this.value != null) {
            return HeaderElement.parseAll(this.value);
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
            
    /**
     * This class represents a raw HTTP header whose content is parsed
     * 'on demand' only when the header value needs to be consumed  
     */
    static class BufferedHeader extends Header {
        
        private final CharArrayBuffer buffer;
        private final int posValue;
        
        private BufferedHeader(final String name, final CharArrayBuffer buffer, int posValue) {
            super(name, null);
            this.buffer = buffer;
            this.posValue = posValue;
        }
        
        public String getValue() {
            return this.buffer.substringTrimmed(this.posValue, this.buffer.length());
        }
        
        public HeaderElement[] getElements() {
            return HeaderElement.parseAll(this.buffer, this.posValue, this.buffer.length());
        }
        
        public String toString() {
            return this.buffer.toString();
        }
        
    }
    
    public static Header[] parseAll(final HttpDataReceiver datareceiver) 
            throws HttpException, IOException {
        ArrayList headerLines = new ArrayList();

        CharArrayBuffer current = null;
        CharArrayBuffer previous = null;
        for (;;) {
            if (current == null) {
                current = new CharArrayBuffer(64);
            } else {
                current.clear();
            }
            int l = datareceiver.readLine(current);
            if (l == -1 || current.length() < 1) {
                break;
            }
            // Parse the header name and value
            // Check for folded headers first
            // Detect LWS-char see HTTP/1.0 or HTTP/1.1 Section 2.2
            // discussion on folded headers
            if ((current.charAt(0) == ' ' || current.charAt(0) == '\t') && previous != null) {
                // we have continuation folded header
                // so append value
                int i = 0;
                while (i < current.length()) {
                    char ch = current.charAt(i);
                    if (ch != ' ' && ch != '\t') {
                        break;
                    }
                    i++;
                }
                previous.append(' ');
                previous.append(current, i, current.length() - i);
            } else {
                headerLines.add(current);
                previous = current;
                current = null;
            }
        }
        Header[] headers = new Header[headerLines.size()];
        for (int i = 0; i < headerLines.size(); i++) {
            CharArrayBuffer buffer = (CharArrayBuffer) headerLines.get(i);
            int colon = buffer.indexOf(':');
            if (colon == -1) {
                throw new ProtocolException("Invalid header: " + buffer.toString());
            }
            String s = buffer.substringTrimmed(0, colon);
            if (s.equals("")) {
                throw new ProtocolException("Invalid header: " + buffer.toString());
            }
            headers[i] = new BufferedHeader(s, buffer, colon + 1);
        }
        return headers;
    }

}
