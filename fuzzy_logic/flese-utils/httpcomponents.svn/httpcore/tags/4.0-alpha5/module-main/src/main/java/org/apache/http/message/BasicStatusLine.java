/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha5/module-main/src/main/java/org/apache/http/message/BasicStatusLine.java $
 * $Revision: 505744 $
 * $Date: 2007-02-10 19:58:45 +0100 (Sat, 10 Feb 2007) $
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

import org.apache.http.HttpStatus;
import org.apache.http.HttpVersion;
import org.apache.http.ProtocolException;
import org.apache.http.StatusLine;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.CharArrayBuffer;
//import org.apache.http.impl.EnglishReasonPhraseCatalog;


/**
 * Represents a status line as returned from a HTTP server.
 * See <a href="http://www.ietf.org/rfc/rfc2616.txt">RFC2616</a> section 6.1.
 * This class is immutable and therefore inherently thread safe.
 *
 * @see HttpStatus
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 *
 * @version $Id: BasicStatusLine.java 505744 2007-02-10 18:58:45Z rolandw $
 * 
 * @since 4.0
 */
public class BasicStatusLine implements StatusLine {

    // ----------------------------------------------------- Instance Variables

    /** The HTTP-Version. */
    private final HttpVersion httpVersion;

    /** The Status-Code. */
    private final int statusCode;

    /** The Reason-Phrase. */
    private final String reasonPhrase;

    // ----------------------------------------------------------- Constructors
    /**
     * Creates a new status line with the given version, status, and reason.
     *
     * @param httpVersion       the HTTP version of the response
     * @param statusCode        the status code of the response
     * @param reasonPhrase      the reason phrase to the status code, or
     *                          <code>null</code>
     */
    public BasicStatusLine(final HttpVersion httpVersion, int statusCode,
                           final String reasonPhrase) {
        super();
        if (httpVersion == null) {
            throw new IllegalArgumentException
                ("HTTP version may not be null.");
        }
        if (statusCode < 0) {
            throw new IllegalArgumentException
                ("Status code may not be negative.");
        }
        this.httpVersion = httpVersion;
        this.statusCode = statusCode;
        this.reasonPhrase = reasonPhrase;
    }

    /**
     * Parses the status line returned from the HTTP server.
     *
     * @param buffer    the buffer from which to parse
     * @param indexFrom where to start parsing in the buffer
     * @param indexTo   where to stop parsing in the buffer
     * 
     * @throws HttpException if the status line is invalid
     */
    public static StatusLine parse(
            final CharArrayBuffer buffer, final int indexFrom, final int indexTo) 
            throws ProtocolException {
        if (buffer == null) {
            throw new IllegalArgumentException("Char array buffer may not be null");
        }
        if (indexFrom < 0) {
            throw new IndexOutOfBoundsException();
        }
        if (indexTo > buffer.length()) {
            throw new IndexOutOfBoundsException();
        }
        if (indexFrom > indexTo) {
            throw new IndexOutOfBoundsException();
        }
        try {
            int i = indexFrom;
            //handle the HTTP-Version
            while (HTTP.isWhitespace(buffer.charAt(i))) {
                i++;
            }            
            int blank = buffer.indexOf(' ', i, indexTo);
            if (blank <= 0) {
                throw new ProtocolException(
                        "Unable to parse HTTP-Version from the status line: "
                        + buffer.substring(indexFrom, indexTo));
            }
            HttpVersion ver = BasicHttpVersionFormat.parse(buffer, i, blank);

            i = blank;
            //advance through spaces
            while (HTTP.isWhitespace(buffer.charAt(i))) {
                i++;
            }            

            //handle the Status-Code
            blank = buffer.indexOf(' ', i, indexTo);
            if (blank < 0) {
                blank = indexTo;
            }
            int statusCode = 0;
            try {
                statusCode = Integer.parseInt(buffer.substringTrimmed(i, blank));
            } catch (NumberFormatException e) {
                throw new ProtocolException(
                    "Unable to parse status code from status line: " 
                    + buffer.substring(indexFrom, indexTo));
            }
            //handle the Reason-Phrase
            i = blank;
            String reasonPhrase = null;
            if (i < indexTo) {
                reasonPhrase = buffer.substringTrimmed(i, indexTo);
            } else {
                reasonPhrase = "";
            }
            return new BasicStatusLine(ver, statusCode, reasonPhrase);
        } catch (IndexOutOfBoundsException e) {
            throw new ProtocolException("Invalid status line: " + 
                    buffer.substring(indexFrom, indexTo)); 
        }
    }

    public static final StatusLine parse(final String s)
            throws ProtocolException {
        if (s == null) {
            throw new IllegalArgumentException("String may not be null");
        }
        CharArrayBuffer buffer = new CharArrayBuffer(s.length()); 
        buffer.append(s);
        return parse(buffer, 0, buffer.length());
    }

    // --------------------------------------------------------- Public Methods

    /**
     * @return the Status-Code
     */
    public int getStatusCode() {
        return this.statusCode;
    }

    /**
     * @return the HTTP-Version
     */
    public HttpVersion getHttpVersion() {
        return this.httpVersion;
    }

    /**
     * @return the Reason-Phrase
     */
    public String getReasonPhrase() {
        return this.reasonPhrase;
    }

    public String toString() {
    	CharArrayBuffer buffer = new CharArrayBuffer(64);        
        buffer.append(this.httpVersion);
        buffer.append(' ');
        buffer.append(Integer.toString(this.statusCode));
        if (this.reasonPhrase != null && this.reasonPhrase.length() > 0) {
            buffer.append(' ');
            buffer.append(this.reasonPhrase);
        }
        return buffer.toString();
    }
    
    public static void format(final CharArrayBuffer buffer, final StatusLine statusline) {
        if (buffer == null) {
            throw new IllegalArgumentException("String buffer may not be null");
        }
        if (statusline == null) {
            throw new IllegalArgumentException("Status line may not be null");
        }
        BasicHttpVersionFormat.format(buffer, statusline.getHttpVersion());
        buffer.append(' ');
        buffer.append(Integer.toString(statusline.getStatusCode()));
        buffer.append(' '); // keep whitespace even if reason phrase is empty
        if (statusline.getReasonPhrase() != null) {
            buffer.append(statusline.getReasonPhrase());
        }
    }
 
    public static String format(final StatusLine statusline) {
        CharArrayBuffer buffer = new CharArrayBuffer(32);
        format(buffer, statusline);
        return buffer.toString();
    }
    
}
