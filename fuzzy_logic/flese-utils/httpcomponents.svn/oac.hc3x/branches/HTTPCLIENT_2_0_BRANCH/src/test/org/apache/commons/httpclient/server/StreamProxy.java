/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/Attic/StreamProxy.java,v 1.1.2.2 2004/02/22 18:21:18 olegk Exp $
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

package org.apache.commons.httpclient.server;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Pipes all data of an input stream through to an output stream asynchronously.
 * Instances of this class are thread safe.
 * 
 * @author Ortwin Glueck
 */
class StreamProxy {
    private InputStream in;
    private OutputStream out;
    private Pump pump = new Pump();
    private Thread pumpThread = new Thread(pump, "Stream copier");
    private int state = 0;
  
    public StreamProxy(InputStream in, OutputStream out) {
        this.in = in;
        this.out = out;    
    }    
    
    public synchronized void start() {
        if (state != 0) throw new IllegalStateException("Can not start again.");
        state = 1;
        pumpThread.start();
    }
    
    /**
     * Returns immediately. The object must not be used again.
     */
    public void abort() {
        if (state != 1)  return;
        state = 2;
        pumpThread.interrupt();
        dispose();
    }
    
    /**
     * Blocks until all data has been copied. Basically calls the 
     * join method on the pump thread.
     * @throws InterruptedException
     */
    public void block() throws InterruptedException {
    	if (state != 1) throw new IllegalStateException("Can not block before started");
    	pumpThread.join();
    }
    
    private void dispose() {
        pumpThread = null;
        pump = null;
        in = null;
        out = null;
    }

    private class Pump implements Runnable {

        public void run() {
            byte[] buffer = new byte[10000];
            try {
                while (!Thread.interrupted()) {
                    int len;
                    while ((len = in.read(buffer)) != -1) {
                        out.write(buffer, 0, len);
                        out.flush();
                    }
                }
            } catch(IOException e) {
                /* expected if parties close connection */
            	e.printStackTrace();
            } finally {
            	dispose();
            }
        }
        
    }
}
