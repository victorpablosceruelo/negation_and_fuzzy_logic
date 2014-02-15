/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/SimpleConnSet.java,v 1.1 2004/11/13 12:21:28 olegk Exp $
 * $Revision: 1.1 $
 * $Date: 2004-11-13 13:21:28 +0100 (Sat, 13 Nov 2004) $
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

package org.apache.commons.httpclient.server;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * A simple set of connections.
 * 
 * @author Oleg Kalnichevski
 */
public class SimpleConnSet {
    
    private static final Log LOG = LogFactory.getLog(SimpleConnSet.class);
    
    private Set connections = new HashSet();
    
    public SimpleConnSet() {
        super();
    }

    public synchronized void addConnection(final SimpleHttpServerConnection conn) {
        this.connections.add(conn);
    }
    
    public synchronized void removeConnection(final SimpleHttpServerConnection conn) {
        this.connections.remove(conn);
    }

    public synchronized void shutdown() {
        for (Iterator i = connections.iterator(); i.hasNext();) {
            SimpleHttpServerConnection conn = (SimpleHttpServerConnection) i.next();
            conn.close();
        }
    }

}
