/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/Attic/URIUtil.java,v 1.1 2001/04/30 18:25:50 remm Exp $
 * $Revision: 1.1 $
 * $Date: 2001-04-30 20:25:50 +0200 (Mon, 30 Apr 2001) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999 The Apache Software Foundation.  All rights 
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:  
 *       "This product includes software developed by the 
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Tomcat", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written 
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
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

package org.apache.commons.httpclient;

import java.io.UnsupportedEncodingException;
import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.IOException;
import java.util.BitSet;


/**
 * General purpose escaping and unescaping utility methods.
 * For "character encoding", The whole escaped characters must be done.
 * It's different between "character encoding" and "escaping of characters".
 *
 * NOTICE: In order to do URI escaping, using the reserved characters defined
 * in this class is not recommended for the the specific protocol.
 *
 * @author Craig R. McClanahan
 * @author Tim Tye
 * @author Remy Maucherat
 * @author Park, Sung-Gu
 * @version $Revision: 1.1 $ $Date: 2001-04-30 20:25:50 +0200 (Mon, 30 Apr 2001) $
 * @see <a href=http://www.ietf.org/rfc/rfc2396.txt?number=2396>RFC 2396</a>
 */

public class URIUtil {

    // -------------------------------------------------------------- Constants

    
    /**
     * Array containing the ASCII expression for hexadecimal.
     */
    private static final char[] hexadecimal =
    {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 
     'A', 'B', 'C', 'D', 'E', 'F'};


    // ----------------------------------------------------- Instance Variables


    /**
     * Array containing the alphanum URI character set.
     */
    public static final BitSet alphanum = new BitSet(128);


    /**
     * Array containing the reserved URI character set of the scheme part.
     */
    public static final BitSet schemeReserved = new BitSet(128);


    /**
     * Array containing the reserved URI character set of the authority part.
     */
    public static final BitSet authorityReserved = new BitSet(128);


    /**
     * Array containing the reserved URI character set of the userinfo part.
     */
    public static final BitSet userinfoReserved = new BitSet(128);


    /**
     * Array containing the reserved URI character set of the host part.
     */
    public static final BitSet hostReserved = new BitSet(128);


    /**
     * Array containing the reserved URI character set of the path part.
     */
    public static final BitSet pathReserved = new BitSet(128);


    /**
     * Array containing the reserved URI character set of the query.
     */
    public static final BitSet queryReserved = new BitSet(128);


    // ----------------------------------------------------- Static Initializer


    static {

        // Save the alphanum URI characters that is common to do URI escaping.
        for (int i = 'a'; i <= 'z'; i++) {
            alphanum.set(i);
        }
        for (int i = 'A'; i <= 'Z'; i++) {
            alphanum.set(i);
        }
        for (int i = '0'; i <= '9'; i++) {
            alphanum.set(i);
        }

        // Save the reserved URI characters within the sheme component.
        /**
         * Actually, this should be any combination of lower case letters,
         * digits, plus ("+"), period ("."), or hyphen ("-").
         * The upper case letters should be treated as equivalent to lower
         * case in scheme names.
         */
        schemeReserved.set('+');
        schemeReserved.set('.');
        schemeReserved.set('-');

        // Save the reserved URI characters within the authority component.
        authorityReserved.set(';');
        authorityReserved.set(':');
        authorityReserved.set('@');
        authorityReserved.set('?');
        authorityReserved.set('/');

        // Save the reserved URI characters within the userinfo component.
        userinfoReserved.set(';');
        userinfoReserved.set(':');
        userinfoReserved.set('&');
        userinfoReserved.set('=');
        userinfoReserved.set('+');
        userinfoReserved.set('$');
        userinfoReserved.set(',');

        // Save the reserved URI characters within the host component.
        hostReserved.set('.');
        hostReserved.set('-');

        // Save the reserved URI characters within the path component.
        pathReserved.set('/');
        pathReserved.set(';');
        pathReserved.set('=');
        pathReserved.set('?');

        // Save the reserved URI characters within the query component.
        queryReserved.set(';');
        queryReserved.set('/');
        queryReserved.set('?');
        queryReserved.set(':');
        queryReserved.set('@');
        queryReserved.set('&');
        queryReserved.set('=');
        queryReserved.set('+');
        queryReserved.set(',');
        queryReserved.set('$');

    }


    // ------------------------------------------------------------ Properties


    /**
     * Get the reserved URI character set of alphanum.
     */
    public static BitSet alphanum() {
        return alphanum;
    }
    

    /**
     * Get the reserved URI character set of the scheme component.
     */
    public static BitSet schemeReserved() {
        return schemeReserved;
    }


    /**
     * Get the reserved URI character set of the authority component.
     */
    public static BitSet authorityReserved() {
        return authorityReserved;
    }


    /**
     * Get the reserved URI character set of the userinfo component.
     */
    public static BitSet userinfoReserved() {
        return userinfoReserved;
    }


    /**
     * Get the reserved URI character set of the host component.
     */
    public static BitSet hostReserved() {
        return hostReserved;
    }


    /**
     * Get the reserved URI character set of the path component.
     */
    public static BitSet pathReserved() {
        return pathReserved;
    }


    /**
     * Get the reserved URI character set of the query component.
     */
    public static BitSet queryReserved() {
        return queryReserved;
    }


    // -------------------------------------------------------- Private Methods


    /**
     * Convert a byte character value to hexidecimal digit value.
     *
     * @param b the character value byte
     */
    private static byte convertHexDigit(byte b) {
        if ((b >= '0') && (b <= '9')) return (byte)(b - '0');
        if ((b >= 'a') && (b <= 'f')) return (byte)(b - 'a' + 10);
        if ((b >= 'A') && (b <= 'F')) return (byte)(b - 'A' + 10);
        return 0;
    }

    
    // --------------------------------------------------------- Public Methods
    
    
    /**
     * Unescape the escaped URI string.
     *
     * @param str The escaped URI string.
     * @exception IllegalArgumentException if a '%' character is not followed
     * by a valid 2-digit hexadecimal number
     */
    public static String unescape(String str) {
        return (str == null) ? null : unescape(str.getBytes());
    }
    

    /**
     * Unescape the escaped URI string.
     *
     * @param bytes The escaped URI byte array.
     * @exception IllegalArgumentException if a '%' character is not followed
     * by a valid 2-digit hexadecimal number
     */
    public static String unescape(byte[] bytes) {
        return unescape(bytes, null);
    }


    /**
     * Unescape the escaped URI string.
     *
     * @param bytes The escaped URI byte array.
     * @exception IllegalArgumentException if a '%' character is not followed
     * by a valid 2-digit hexadecimal number
     */
    public static String unescape(byte[] bytes, int off, int len) {
        return unescape(bytes, off, len, null);
    }


    /**
     * Unescape the escaped URI string with character encoding.
     *
     * @param bytes The escaped URI byte array.
     * @param enc The encoding to use.
     *            If null or wrong, the default encoding is used.
     * @exception IllegalArgumentException if a '%' character is not followed
     * by a valid 2-digit hexadecimal number
     */
    public static String unescape(byte[] bytes, String enc) {
        if (bytes == null)
            return (null);
        return unescape(bytes, 0, bytes.length, enc);
    }


    /**
     * Unescape the escaped URI string with character encoding.
     *
     * @param bytes The escaped URI byte array.
     * @param enc The encoding to use.
     *            If null or wrong, the default encoding is used.
     * @exception IllegalArgumentException if a '%' character is not followed
     * by a valid 2-digit hexadecimal number
     */
    public static String unescape(byte[] bytes, int off, int len, String enc) {
        
        if (bytes == null)
            return (null);
        
        int end = off + len;
        int ix = off;
        int ox = off;
        while (ix < end) {
            byte b = bytes[ix++];     // Get byte to test
            if (b == '+') {
                b = (byte) ' ';
            } else if (b == '%') {
                b = (byte) ((convertHexDigit(bytes[ix++]) << 4)
                            + convertHexDigit(bytes[ix++]));
            }
            bytes[ox++] = b;
        }
        if (enc != null) {
            try {
                return new String(bytes, off, ox, enc);
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
        }

        return new String(bytes, off, ox);

    }


    /**
     * Escape the unescaped URI string.
     * 
     * @param str The unescaped URI string which has to be rewritten.
     */
    public static String escape(String str) {
        return escape(str, null);
    }


    /**
     * Escape the unescaped URI string.
     * 
     * @param str The unescaped URI string which has to be rewritten.
     * @param reserved The additional reserved URI character set.
     */
    public static String escape(String str, BitSet reserved) {
        return (str == null) ? null : escape(str.getBytes(), reserved);
    }


    /**
     * Escape the unescaped URI byte array.
     * 
     * @param bytes The unescaped URI byte array which has to be rewritten.
     * @param reserved The additional reserved URI character set.
     */
    public static String escape(byte[] bytes, BitSet reserved) {
        return (bytes == null) ? null 
            : escape(bytes, 0, bytes.length, reserved);
    }


    /**
     * Escape the unescaped URI byte array.
     * 
     * @param bytes The unescaped URI byte array which has to be rewritten.
     * @param reserved The additional reserved URI character set.
     */
    public static String escape(byte[] bytes, int off, 
                                int len, BitSet reserved) {
        
        if (bytes == null)
            return (null);
        
        StringBuffer rewrittenStr = new StringBuffer(len);

        for (int i = off; i < len; i++) {
            char c = (char) bytes[i];
            if (alphanum.get(c)) {
                rewrittenStr.append(c);
            } else if (reserved != null && reserved.get(c)) {
                rewrittenStr.append(c);
            } else {
                byte toEscape = bytes[i];
                rewrittenStr.append('%');
                int low = (int) (toEscape & 0x0f);
                int high = (int) ((toEscape & 0xf0) >> 4);
                rewrittenStr.append(hexadecimal[high]);
                rewrittenStr.append(hexadecimal[low]);
            }
        }
        
        return rewrittenStr.toString();
    }


    /**
     * Escape the unescaped URI string with character encoding.
     *
     * @param str The string which has to be rewiten.
     * @param reserved The additional reserved URI character set.
     * @param enc The encoding to use.
     *            If wrong, the default encoding is used.
     */
    public static String escape(String str, BitSet reserved, String enc) {
        try {
            return escape(str.getBytes(enc), reserved);
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            return escape(str.getBytes(), reserved);
        }
    }

}

