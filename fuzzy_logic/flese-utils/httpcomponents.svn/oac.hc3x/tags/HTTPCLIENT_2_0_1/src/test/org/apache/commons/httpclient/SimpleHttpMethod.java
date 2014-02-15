/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/Attic/SimpleHttpMethod.java,v 1.6.2.1 2004/02/22 18:21:16 olegk Exp $
 * $Revision: 1.6.2.1 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
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


package org.apache.commons.httpclient;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.IOException;


/** 
 * For test-nohost testing purposes only.
 *
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 */
class SimpleHttpMethod extends HttpMethodBase{

    static Log log = LogFactory.getLog("httpclient.test");
    Header header = null;

	SimpleHttpMethod(){
		super();
	}

	SimpleHttpMethod(String path){
		super(path);
	}

	SimpleHttpMethod(Header header){
		super();
        this.header = header;
	}

	public String getName() {
		return "Simple";
	}

    /**
     * Makes sure any respose header that exists has been added to the response
     * header group.
     */
    private void ensureResponseHeaderIsSet() {
        if ( header != null ) {
            super.getResponseHeaderGroup().addHeader(header);
            header = null;
        }
    }

    /**
     * @see HttpMethod#execute(HttpState, HttpConnection)
     */
    public int execute(HttpState state, HttpConnection connection)
        throws HttpException, IOException {
        return super.execute(state, connection);
    }

    /**
     * @see HttpMethod#getResponseHeader(String)
     * @deprecated
     */
    public Header getResponseHeader(String headerName) {
        ensureResponseHeaderIsSet();
        return super.getResponseHeader(headerName);
    }

    /**
     * @see HttpMethod#getResponseHeaderGroup()
     */
    protected HeaderGroup getResponseHeaderGroup() {
        ensureResponseHeaderIsSet();
        return super.getResponseHeaderGroup();
    }

    /**
     * @see HttpMethod#getResponseHeaders()
     */
    public Header[] getResponseHeaders() {
        ensureResponseHeaderIsSet();
        return super.getResponseHeaders();
    }
    
    
    public String getTestRequestLine(HttpConnection connection) {
        return HttpMethodBase.generateRequestLine(connection, 
          this.getName(), this.getPath(), this.getQueryString(), "HTTP/1.1");
    }
}
