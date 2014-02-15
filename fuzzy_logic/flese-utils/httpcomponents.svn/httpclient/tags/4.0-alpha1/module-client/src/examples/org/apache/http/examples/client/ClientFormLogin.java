/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/examples/org/apache/http/examples/client/ClientFormLogin.java $
 * $Revision: 554349 $
 * $Date: 2007-07-08 13:45:20 +0200 (Sun, 08 Jul 2007) $
 * ====================================================================
 *
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
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

package org.apache.http.examples.client;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.UrlEncodedFormEntity;
import org.apache.http.client.params.CookiePolicy;
import org.apache.http.client.params.HttpClientParams;
import org.apache.http.cookie.Cookie;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;

/**
 * A example that demonstrates how HttpClient APIs can be used to perform 
 * form-based logon.
 */
public class ClientFormLogin {

    public static void main(String[] args) throws Exception {

        DefaultHttpClient httpclient = new DefaultHttpClient();
        httpclient.getParams().setParameter(
                HttpClientParams.COOKIE_POLICY, CookiePolicy.BROWSER_COMPATIBILITY);

        HttpGet httpget = new HttpGet("https://portal.sun.com/portal/dt");

        HttpResponse response = httpclient.execute(httpget);
        HttpEntity entity = response.getEntity();
        
        System.out.println("Login form get: " + response.getStatusLine()); 
        if (entity != null) {
            entity.consumeContent();
        }
        System.out.println("Initial set of cookies:");    
        Cookie[] cookies = httpclient.getState().getCookies();
        if (cookies.length == 0) {
            System.out.println("None");    
        } else {
            for (int i = 0; i < cookies.length; i++) {
                System.out.println("- " + cookies[i].toString());    
            }
        }
        
        HttpPost httpost = new HttpPost("https://portal.sun.com/amserver/UI/Login?" +
        		"org=self_registered_users&" +
        		"goto=/portal/dt&" +
        		"gotoOnFail=/portal/dt?error=true");
        
        NameValuePair[] nvps = new NameValuePair[] {
                new BasicNameValuePair("IDToken1", "username"),
                new BasicNameValuePair("IDToken2", "password")
        };
        
        httpost.setEntity(new UrlEncodedFormEntity(nvps, HTTP.UTF_8));
        
        response = httpclient.execute(httpost);
        entity = response.getEntity();
        
        System.out.println("Login form get: " + response.getStatusLine()); 
        if (entity != null) {
            entity.consumeContent();
        }
        
        System.out.println("Post logon cookies:");    
        cookies = httpclient.getState().getCookies();
        if (cookies.length == 0) {
            System.out.println("None");    
        } else {
            for (int i = 0; i < cookies.length; i++) {
                System.out.println("- " + cookies[i].toString());    
            }
        }
    }
}
