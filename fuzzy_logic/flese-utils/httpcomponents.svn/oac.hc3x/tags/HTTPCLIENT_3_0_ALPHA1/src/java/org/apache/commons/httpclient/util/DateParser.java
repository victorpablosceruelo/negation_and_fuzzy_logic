/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/util/DateParser.java,v 1.9 2004/04/18 23:51:38 jsdever Exp $
 * $Revision: 1.9 $
 * $Date: 2004-04-19 01:51:38 +0200 (Mon, 19 Apr 2004) $
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
 */

package org.apache.commons.httpclient.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.httpclient.params.DefaultHttpParams;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * A utility class for parsing HTTP dates as used in cookies and other headers.  
 * This class handles dates as defined by RFC 2616 section 3.3.1 as well as 
 * some other common non-standard formats.
 * 
 * @author Christopher Brown
 * @author Michael Becke
 */
public class DateParser {

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(DateParser.class);

    /**
     * The key used to look up the date patterns used for parsing.  The String patterns are stored
     * in a {@link Collection} and must be compatible with {@link SimpleDateFormat}.
     * 
     * @see org.apache.commons.httpclient.params.DefaultHttpParams
     */
    public static final String KEY_DATE_PATTERNS = "http.dateParser.patterns";

    /**
     * Date format pattern used to parse HTTP date headers in RFC 1123 format.
     */
    public static final String PATTERN_RFC1123 = "EEE, dd MMM yyyy HH:mm:ss zzz";

    /**
     * Date format pattern used to parse HTTP date headers in RFC 1036 format.
     */
    public static final String PATTERN_RFC1036 = "EEEE, dd-MMM-yy HH:mm:ss zzz";

    /**
     * Date format pattern used to parse HTTP date headers in ANSI C 
     * <code>asctime()</code> format.
     */
    public static final String PATTERN_ASCTIME = "EEE MMM d HH:mm:ss yyyy";

    /**
     * Parses a date value.  The formats used for parsing the date value are retrieved from
     * the default http params.
     *
     * @param dateValue the date value to parse
     * 
     * @return the parsed date
     *
     * @throws DateParseException if the value could not be parsed using any of the 
     * supported date formats
     * 
     * @see DefaultHttpParams#getDefaultParams()
     */
    public static Date parseDate(String dateValue) throws DateParseException {
        
        Collection patterns = (Collection) DefaultHttpParams.getDefaultParams().getParameter(
            KEY_DATE_PATTERNS
        );
        if (patterns == null) {
            LOG.warn("DateParser patterns not included in the default params.");
            patterns = Arrays.asList(
                new String[] { PATTERN_ASCTIME, PATTERN_RFC1036, PATTERN_RFC1123 }
            );
        }
        return parseDate(dateValue, patterns);
    }
    
    /**
     * Parses the date value using the given date formats.
     * 
     * @param dateValue the date value to parse
     * @param dateFormats the date formats to use
     * 
     * @return the parsed date
     * 
     * @throws DateParseException if none of the dataFormats could parse the dateValue
     */
    private static Date parseDate(
        String dateValue, 
        Collection dateFormats
    ) throws DateParseException {
        
        if (dateValue == null) {
            throw new IllegalArgumentException("dateValue is null");
        }

        // trim single quotes around date if present
        // see http://nagoya.apache.org/bugzilla/show_bug.cgi?id=5279
        if (dateValue.length() > 1 
            && dateValue.startsWith("'") 
            && dateValue.endsWith("'")
        ) {
            dateValue = dateValue.substring (1, dateValue.length() - 1);
        }
        
        SimpleDateFormat dateParser = null;        
        Iterator formatIter = dateFormats.iterator();
        
        while (formatIter.hasNext()) {
            String format = (String) formatIter.next();            
            if (dateParser == null) {
                dateParser = new SimpleDateFormat(format, Locale.US);
                dateParser.setTimeZone(TimeZone.getTimeZone("GMT"));
            } else {
                dateParser.applyPattern(format);                    
            }
            try {
                return dateParser.parse(dateValue);
            } catch (ParseException pe) {
                // ignore this exception, we will try the next format
            }                
        }
        
        // we were unable to parse the date
        throw new DateParseException("Unable to parse the date " + dateValue);        
    }

    /** This class should not be instantiated. */    
    private DateParser() { }
    
}
