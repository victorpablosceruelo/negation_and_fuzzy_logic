/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/main/java/org/apache/http/impl/DefaultHttpClientConnection.java $
 * $Revision: 453172 $
 * $Date: 2006-10-05 12:12:32 +0200 (Thu, 05 Oct 2006) $
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

package org.apache.http.impl;

import java.io.IOException;
import java.net.Socket;

import org.apache.http.params.HttpParams;

/**
 * Default implementation of a client-side HTTP connection.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 453172 $
 * 
 * @since 4.0
 */
public class DefaultHttpClientConnection extends SocketHttpClientConnection {

    public DefaultHttpClientConnection() {
        super();
    }
    
    public void bind(
            final Socket socket, 
            final HttpParams params) throws IOException {
        super.bind(socket, params);
    }

}
