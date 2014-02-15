/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/params/HostParams.java,v 1.2 2004/05/13 04:01:22 mbecke Exp $
 * $Revision: 1.2 $
 * $Date: 2004-05-13 06:03:25 +0200 (Thu, 13 May 2004) $
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

package org.apache.commons.httpclient.params;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * This class represents a collection of HTTP protocol parameters applicable to 
 * {@link org.apache.commons.httpclient.HostConfiguration instances of HostConfiguration}. 
 * Protocol parameters may be linked together to form a hierarchy. If a particular 
 * parameter value has not been explicitly defined in the collection itself, its 
 * value will be drawn from the parent collection of parameters.
 * 
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 1.2 $
 * 
 * @since 3.0
 */
public class HostParams extends DefaultHttpParams {

    /** Log object for this class. */
    private static final Log LOG = LogFactory.getLog(HttpParams.class);

    /**
     * Creates a new collection of parameters with the collection returned
     * by {@link #getDefaultParams()} as a parent. The collection will defer
     * to its parent for a default value if a particular parameter is not 
     * explicitly set in the collection itself.
     * 
     * @see #getDefaultParams()
     */
    public HostParams() {
        super();
    }

    /**
     * Creates a new collection of parameters with the given parent. 
     * The collection will defer to its parent for a default value 
     * if a particular parameter is not explicitly set in the collection
     * itself.
     * 
     * @param defaults the parent collection to defer to, if a parameter
     * is not explictly set in the collection itself.
     *
     * @see #getDefaultParams()
     */
    public HostParams(HttpParams defaults) {
        super(defaults);
    }
}
