/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0-alpha2/src/examples/org/apache/http/examples/ElementalHttpPost.java $
 * $Revision: 376458 $
 * $Date: 2006-02-09 23:22:06 +0100 (Thu, 09 Feb 2006) $
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

package org.apache.http.examples;

import java.io.ByteArrayInputStream;

import org.apache.http.ConnectionReuseStrategy;
import org.apache.http.HttpClientConnection;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHost;
import org.apache.http.HttpResponse;
import org.apache.http.HttpVersion;
import org.apache.http.Scheme;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.entity.InputStreamEntity;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.impl.DefaultHttpClientConnection;
import org.apache.http.impl.DefaultHttpParams;
import org.apache.http.impl.io.PlainSocketFactory;
import org.apache.http.io.SocketFactory;
import org.apache.http.message.HttpPost;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.protocol.HttpRequestExecutor;
import org.apache.http.protocol.RequestConnControl;
import org.apache.http.protocol.RequestContent;
import org.apache.http.protocol.RequestExpectContinue;
import org.apache.http.protocol.RequestTargetHost;
import org.apache.http.protocol.RequestUserAgent;
import org.apache.http.util.EntityUtils;

/**
 * <p>
 * </p>
 * @author <a href="mailto:oleg at ural.ru">Oleg Kalnichevski</a>
 *
 * @version $Revision: 376458 $
 */
public class ElementalHttpPost {

    public static void main(String[] args) throws Exception {
        
        SocketFactory socketfactory = PlainSocketFactory.getSocketFactory();
        Scheme.registerScheme("http", new Scheme("http", socketfactory, 80));

        HttpParams params = new DefaultHttpParams(null);
        HttpProtocolParams.setVersion(params, HttpVersion.HTTP_1_1);
        HttpProtocolParams.setContentCharset(params, "UTF-8");
        HttpProtocolParams.setUserAgent(params, "Jakarta-HttpComponents/1.1");
        HttpProtocolParams.setUseExpectContinue(params, true);
        
        HttpRequestExecutor httpexecutor = new HttpRequestExecutor();
        httpexecutor.setParams(params);
        // Required protocol interceptors
        httpexecutor.addInterceptor(new RequestContent());
        httpexecutor.addInterceptor(new RequestTargetHost());
        // Recommended protocol interceptors
        httpexecutor.addInterceptor(new RequestConnControl());
        httpexecutor.addInterceptor(new RequestUserAgent());
        httpexecutor.addInterceptor(new RequestExpectContinue());
        
        HttpHost host = new HttpHost("localhost", 8080);
        HttpClientConnection conn = new DefaultHttpClientConnection(host);
        try {
            
            HttpEntity[] requestBodies = {
                    new StringEntity(
                            "This is the first test request", "UTF-8"),
                    new ByteArrayEntity(
                            "This is the second test request".getBytes("UTF-8")),
                    new InputStreamEntity(
                            new ByteArrayInputStream(
                                    "This is the third test request (will be chunked)"
                                    .getBytes("UTF-8")), -1)
            };
            
            ConnectionReuseStrategy connStrategy = new DefaultConnectionReuseStrategy();
            
            for (int i = 0; i < requestBodies.length; i++) {
                HttpPost request = new HttpPost("/servlets-examples/servlet/RequestInfoExample");
                request.setEntity(requestBodies[i]);
                System.out.println(">> Request URI: " + request.getRequestLine().getUri());
                HttpResponse response = httpexecutor.execute(request, conn);
                System.out.println("<< Response: " + response.getStatusLine());
                System.out.println(EntityUtils.toString(response.getEntity()));
                System.out.println("==============");
                if (!connStrategy.keepAlive(response)) {
                    conn.close();
                } else {
                    System.out.println("Connection kept alive...");
                }
            }
        } finally {
            conn.close();
        }        
    }
    
}
