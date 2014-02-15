/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/server/Attic/BidiStreamProxy.java,v 1.1.2.1 2003/12/05 21:02:52 oglueck Exp $
 * $Revision: 1.1.2.1 $
 * $Date: 2003-12-05 22:02:52 +0100 (Fri, 05 Dec 2003) $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
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

import java.io.InputStream;
import java.io.OutputStream;

/**
 * Pumps data between a pair of input / output streams. Used to
 * connect the two ends (left and right) of a bidirectional communication channel.
 * Instances of this class are thread safe.
 * 
 * @author Ortwin Glueck
 */
class BidiStreamProxy {
    private StreamProxy leftToRight, rightToLeft;
    private int state = 0;
    
    /**
     * Sets up a new connection between two peers (left and right)
     * @param leftIn input channel of the left peer
     * @param leftOut output channel of the left peer
     * @param rightIn input channel of the right peer
     * @param rightOut output channel of the right peer
     */
    public BidiStreamProxy(InputStream leftIn, OutputStream leftOut, InputStream rightIn, OutputStream rightOut) {
        leftToRight = new StreamProxy(leftIn, rightOut);
        rightToLeft = new StreamProxy(rightIn, leftOut);
    }
    
    /**
     * Starts pumping the information from left to right and vice versa.
     * This is performed asynchronously so this method returns immediately.
     */
    public synchronized void start() {
        if (state != 0) throw new IllegalStateException("Can not start twice");
        leftToRight.start();
        rightToLeft.start();
        state = 1;
    }
    
    /**
     * Aborts the communication between the peers and releases all resources.
     * Note: The method does not wait for the pump threads to terminate.
     */
    public synchronized void abort() {
        if (leftToRight != null) leftToRight.abort();
        if (rightToLeft != null)  rightToLeft.abort();
        leftToRight = null;
        rightToLeft = null;
    }
    
    /**
     * Blocks until all data has been copied. Basically calls the 
     * join method on the pump thread.
     * @throws InterruptedException
     */
    public void block() throws InterruptedException {
    	if (state != 1) throw new IllegalStateException("Can not block before started");
    	leftToRight.block();
    	rightToLeft.block();
    }
}
