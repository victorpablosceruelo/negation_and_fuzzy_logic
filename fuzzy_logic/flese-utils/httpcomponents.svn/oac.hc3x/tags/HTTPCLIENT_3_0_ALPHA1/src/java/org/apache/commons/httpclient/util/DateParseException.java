/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/util/DateParseException.java,v 1.4 2004/04/18 23:51:38 jsdever Exp $
 * $Revision: 1.4 $
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


/**
 * An exception to indicate an error parsing a date string.
 * 
 * @see DateParser
 * 
 * @author Michael Becke
 */
public class DateParseException extends Exception {

    /**
     * 
     */
    public DateParseException() {
        super();
    }

    /**
     * @param message the exception message
     */
    public DateParseException(String message) {
        super(message);
    }

}
