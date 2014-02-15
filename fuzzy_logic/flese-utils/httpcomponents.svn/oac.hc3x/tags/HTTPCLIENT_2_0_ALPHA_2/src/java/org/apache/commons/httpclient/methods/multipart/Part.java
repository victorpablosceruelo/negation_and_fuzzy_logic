/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/methods/multipart/Part.java,v 1.6 2003/01/25 00:59:11 jsdever Exp $
 * $Revision: 1.6 $
 * $Date: 2003-01-25 01:59:11 +0100 (Sat, 25 Jan 2003) $
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "HttpClient", and "Apache Software
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

package org.apache.commons.httpclient.methods.multipart;

import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import org.apache.commons.httpclient.HttpConstants;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Abstract class for one Part of a multipart post object.
 *
 * @author <a href="mailto:mattalbright@yahoo.com">Matthew Albright</a>
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @author <a href="mailto:adrian@ephox.com">Adrian Sutton</a>
 *
 * @since 2.0
 */
public abstract class Part {

    /** Log object for this class. */
    private static final Log log = LogFactory.getLog(Part.class);

    //TODO: Make this configurable
    static String boundary = "----------------314159265358979323846";
    static byte[] boundary_bytes = HttpConstants.getBytes(boundary);
    static String CRLF = "\r\n";
    static byte[] CRLF_bytes = HttpConstants.getBytes(CRLF);
    static String extra = "--";
    static byte[] extra_bytes = HttpConstants.getBytes(extra);
    
    public static String getBoundary() {
        return boundary;
    }
    
    public static void sendLastBoundary(OutputStream out)
    throws IOException {
        log.trace("enter sendLastBoundary(OutputStream out)");
        out.write(extra_bytes);
        out.write(boundary_bytes);
        out.write(extra_bytes);
        out.write(CRLF_bytes);
    }
    
    public static int lengthOfLastBoundary()
    throws IOException {
        log.trace("enter lengthOfLastBoundary()");
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        
        sendLastBoundary(out);
        
        return out.size();
    }
    
    public abstract String getName();
    
    protected void sendStart(OutputStream out) 
    throws IOException {
        log.trace("enter sendStart(OutputStream out)");
        out.write(extra_bytes);
        out.write(boundary_bytes);
        out.write(CRLF_bytes);
    }
    
    protected int lengthOfStart()
    throws IOException {
        log.trace("enter lengthOfStart()");
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        sendStart(out);
        return out.size();
    }
    
    protected void sendHeader(OutputStream out) 
    throws IOException {
        log.trace("enter sendHeader(OutputStream out)");
        String content_dispos = "Content-Disposition: form-data; name=\"" 
            + getName() + "\"";
    
        out.write(HttpConstants.getBytes(content_dispos));
    }
    
    protected int lengthOfHeader()
    throws IOException {
        log.trace("enter lengthOfHeader()");
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        sendHeader(out);
        return(out.size());
    }
    
    
    protected void sendEndOfHeader(OutputStream out) 
    throws IOException {
        log.trace("enter sendEndOfHeader(OutputStream out)");
        out.write(CRLF_bytes);
        out.write(CRLF_bytes);
    }
    
    protected int lengthOfEndOfHeader()
    throws IOException {
        log.trace("enter lengthOfEndOfHeader()");
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        sendEndOfHeader(out);
        return out.size();
    }
    
    
    protected abstract void sendData(OutputStream out) throws IOException;
    
    protected abstract long lengthOfData() throws IOException;
    
    protected void sendEnd(OutputStream out) 
    throws IOException {
        log.trace("enter sendEnd(OutputStream out)");
        out.write(CRLF_bytes);
    }
    
    protected int lengthOfEnd()
    throws IOException {
        log.trace("enter lengthOfEnd()");
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        sendEnd(out);
        return out.size();
    }
    
    /* The following 2 methods don't need to be final, but they DO need
     * to be overridden as a pair, and the only way to make sure of that
     * is to make sure they AREN'T overridden. 
     */

    public final void send(OutputStream out) throws IOException {
        log.trace("enter send(OutputStream out)");
        sendStart(out);
        sendHeader(out);
        sendEndOfHeader(out);
        sendData(out);
        sendEnd(out);
    }
    
    public final long length() throws IOException {
        log.trace("enter length()");
        return lengthOfStart()
               + lengthOfHeader()
               + lengthOfEndOfHeader()
               + lengthOfData()
               + lengthOfEnd();
    }
    
    public String toString() {
        return this.getName();
    }
}
