/*
 * $HeadRL$
 * $Revision: 413281 $
 * $Date: 2006-06-10 12:36:20 +0200 (Sat, 10 Jun 2006) $
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

package org.apache.commons.httpclient;

/**
 * Signals violation of HTTP specification caused by an invalid redirect
 * location
 * 
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 * 
 * @since 3.1
 */
public class InvalidRedirectLocationException extends RedirectException {

    private final String location;
    
    /**
     * Creates a new InvalidRedirectLocationException with the specified detail message.
     * 
     * @param message the exception detail message
     * @param location redirect location
     */
    public InvalidRedirectLocationException(final String message, final String location) {
        super(message);
        this.location = location;
    }

    /**
     * Creates a new RedirectException with the specified detail message and cause.
     * 
     * @param message the exception detail message
     * @param location redirect location
     * @param cause the <tt>Throwable</tt> that caused this exception, or <tt>null</tt>
     * if the cause is unavailable, unknown, or not a <tt>Throwable</tt>
     */
    public InvalidRedirectLocationException(final String message, final String location, 
            final Throwable cause) {
        super(message, cause);
        this.location = location;
    }
    
    public String getLocation() {
        return this.location;
    }
    
}
