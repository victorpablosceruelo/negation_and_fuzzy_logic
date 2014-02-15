/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha3/module-main/src/main/java/org/apache/http/HttpConnection.java $
 * $Revision: 434476 $
 * $Date: 2006-08-24 21:27:19 +0200 (Thu, 24 Aug 2006) $
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

package org.apache.http;

import java.io.IOException;

/**
 * A generic HTTP connection, useful on client and server side.
 *
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 434476 $
 * 
 * @since 4.0
 */
public interface HttpConnection {

    /**
     * Closes this connection gracefully.
     * This method will attempt to  flush the transmitter's
     * internal buffer prior to closing the underlying socket.
     * This method MUST NOT be called from a different thread to force 
     * shutdown of the connection. Use {@link #shutdown shutdown} instead.
     */
    public void close() throws IOException;
    
    /**
     * Checks if this connection is open.
     * @return true if it is open, false if it is closed.
     */
    public boolean isOpen();
 
    /**
     * Checks whether this connection has gone down.
     * Network connections may get closed during some time of inactivity
     * for several reasons. The next time a read is attempted on such a
     * connection it will throw an IOException.
     * This method tries to alleviate this inconvenience by trying to
     * find out if a connection is still usable. Implementations may do
     * that by attempting a read with a very small timeout. Thus this
     * method may block for a small amount of time before returning a result. 
     * It is therefore an <i>expensive</i> operation.
     * 
     * @return  <code>true</code> if attempts to use this connection are
     *          likely to succeed, or <code>false</code> if they are likely
     *          to fail and this connection should be closed
     */
    public boolean isStale();
    
    /**
     * Force-closes this connection.
     * This is the only method of a connection which may be called
     * from a different thread to terminate the connection. 
     * This method will not attempt to flush the transmitter's
     * internal buffer prior to closing the underlying socket.
     */
    public void shutdown() throws IOException;
}
