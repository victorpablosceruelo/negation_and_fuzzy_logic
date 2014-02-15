/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha2/src/java/org/apache/http/util/HeaderUtils.java $
 * $Revision: 411100 $
 * $Date: 2006-06-02 11:12:04 +0200 (Fri, 02 Jun 2006) $
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

package org.apache.http.util;

import java.io.IOException;
import java.util.ArrayList;

import org.apache.http.Header;
import org.apache.http.HeaderElement;
import org.apache.http.HttpException;
import org.apache.http.ProtocolException;
import org.apache.http.io.CharArrayBuffer;
import org.apache.http.io.HttpDataReceiver;

/**
 * A utility class for processing HTTP headers.
 * 
 * @author Michael Becke
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 */
public class HeaderUtils {
    
    private HeaderUtils() {
    }

    /**
     * This class represents a raw HTTP header whose content is parsed
     * 'on demand' only when the header value needs to be consumed  
     */
    static class BufferedHeader extends Header {
        
        private final CharArrayBuffer buffer;
        private final int posValue;
        
        public BufferedHeader(final String name, final CharArrayBuffer buffer, int posValue) {
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
    
    /**
     * Parses HTTP headers from the data receiver stream according to the generic 
     * format as given in Section 3.1 of RFC 822, RFC-2616 Section 4 and 19.3.
     *  
     * @param datareceiver HTTP data receiver
     * @param maxCount maximum number of headers allowed. If the number of headers 
     *        received from the data stream exceeds maxCount value, an IOException 
     *        will be thrown. Setting this parameter to a negative value or zero 
     *        will disable the check.   
     * @return array of HTTP headers
     * 
     * @throws HttpException
     * @throws IOException
     */
    public static Header[] parseHeaders(final HttpDataReceiver datareceiver, int maxCount) 
            throws HttpException, IOException {
        if (datareceiver == null) {
            throw new IllegalArgumentException("HTTP data receiver may not be null");
        }
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
            if (maxCount > 0 && headerLines.size() >= maxCount) {
                throw new IOException("Maximum header count exceeded");
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

    public static Header[] parseHeaders(final HttpDataReceiver datareceiver) 
        throws HttpException, IOException {
        return parseHeaders(datareceiver, -1);
    }
    
}