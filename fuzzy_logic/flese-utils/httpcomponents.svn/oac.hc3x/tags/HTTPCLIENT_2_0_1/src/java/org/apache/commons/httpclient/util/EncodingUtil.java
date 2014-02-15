/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/util/EncodingUtil.java,v 1.1.2.2 2004/02/22 18:21:16 olegk Exp $
 * $Revision: 1.1.2.2 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
 * ====================================================================
 *
 *  Copyright 1999-2004 The Apache Software Foundation
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
package org.apache.commons.httpclient.util;

import java.util.BitSet;

import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * The home for utility methods that handle various encoding tasks.
 * 
 * @author Michael Becke
 * 
 * @since 2.0 final
 */
public class EncodingUtil {

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(EncodingUtil.class);

    /**
     * BitSet of www-form-url safe characters.
     */
    private static final BitSet WWW_FORM_URL = new BitSet(256);

    // Static initializer for www_form_url
    static {
        // alpha characters
        for (int i = 'a'; i <= 'z'; i++) {
            WWW_FORM_URL.set(i);
        }
        for (int i = 'A'; i <= 'Z'; i++) {
            WWW_FORM_URL.set(i);
        }
        // numeric characters
        for (int i = '0'; i <= '9'; i++) {
            WWW_FORM_URL.set(i);
        }
        // blank to be replaced with +
        WWW_FORM_URL.set(' ');
        WWW_FORM_URL.set('-');
        WWW_FORM_URL.set('_');
        WWW_FORM_URL.set('.');
        WWW_FORM_URL.set('*');
    }
    
    /**
     * Form-urlencoding routine.
     *
     * The default encoding for all forms is `application/x-www-form-urlencoded'. 
     * A form data set is represented in this media type as follows:
     *
     * The form field names and values are escaped: space characters are replaced 
     * by `+', and then reserved characters are escaped as per [URL]; that is, 
     * non-alphanumeric characters are replaced by `%HH', a percent sign and two 
     * hexadecimal digits representing the ASCII code of the character. Line breaks, 
     * as in multi-line text field values, are represented as CR LF pairs, i.e. `%0D%0A'.
     * 
     * @param pairs the values to be encoded
     * @param charset the character set of pairs to be encoded
     * 
     * @return the urlencoded pairs
     * 
     * @since 2.0 final
     */
    public static String formUrlEncode(NameValuePair[] pairs, String charset) {
        
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < pairs.length; i++) {
            if (pairs[i].getName() != null) {
                if (i > 0) {
                    buf.append("&");
                }
                String queryName = pairs[i].getName();
                try {
                    queryName = URIUtil.encode(queryName, WWW_FORM_URL, charset).replace(' ', '+');
                } catch (URIException urie) {
                    LOG.error("Error encoding pair name: " + queryName, urie);
                }
                buf.append(queryName);
                buf.append("=");
                if (pairs[i].getValue() != null) {
                    String queryValue = pairs[i].getValue();
                    try {
                        queryValue = URIUtil.encode(
                            queryValue, 
                            WWW_FORM_URL, 
                            charset
                        ).replace(' ', '+');
                    } catch (URIException urie) {
                        LOG.error("Error encoding pair value: " + queryValue, urie);
                    }
                    buf.append(queryValue);
                }
            }
        }
        return buf.toString();
    }
    
    /**
     * This class should not be instantiated.
     */
    private EncodingUtil() {
    }

}
