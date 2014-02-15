/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/params/HttpConnectionManagerParams.java,v 1.6 2004/05/13 04:01:22 mbecke Exp $
 * $Revision: 1.6 $
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

/**
 * This class represents a collection of HTTP protocol parameters applicable to 
 * {@link org.apache.commons.httpclient.HttpConnectionManager HTTP connection managers}. 
 * Protocol parameters may be linked together to form a hierarchy. If a particular 
 * parameter value has not been explicitly defined in the collection itself, its 
 * value will be drawn from the parent collection of parameters.
 * 
 * @author <a href="mailto:oleg@ural.ru">Oleg Kalnichevski</a>
 * 
 * @version $Revision: 1.6 $
 * 
 * @since 3.0
 */
public class HttpConnectionManagerParams extends HttpConnectionParams {

    /** 
     * Defines the maximum number of connections allowed per host. 
     * <p>
     * This parameter expects a value of type {@link Integer}.
     * </p>
     */
    public static String MAX_HOST_CONNECTIONS = "http.connection-manager.max-per-host";

    /** 
     * Defines the maximum number of connections allowed overall.
     * <p>
     * This parameter expects a value of type {@link Integer}.
     * </p>
     */
    public static String MAX_TOTAL_CONNECTIONS = "http.connection-manager.max-total";

}
