/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/main/java/org/apache/http/protocol/SyncHttpExecutionContext.java $
 * $Revision: 426888 $
 * $Date: 2006-07-30 16:41:59 +0200 (Sun, 30 Jul 2006) $
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

package org.apache.http.protocol;

/**
 * Thread-safe extension of the {@link HttpExecutionContext}.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 426888 $
 * 
 * @since 4.0
 */
public class SyncHttpExecutionContext extends HttpExecutionContext {
    
    public SyncHttpExecutionContext(final HttpContext parentContext) {
        super(parentContext);
    }
    
    public synchronized Object getAttribute(final String id) {
        return super.getAttribute(id);
    }

    public synchronized void setAttribute(final String id, final Object obj) {
        super.setAttribute(id, obj);
    }
    
    public synchronized Object removeAttribute(final String id) {
        return super.removeAttribute(id);
    }

}
