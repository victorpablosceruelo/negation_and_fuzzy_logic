/*
 * $Header: $
 * $Revision: $
 * $Date: $
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

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.methods.TraceMethod;

/**
 * 
 * Simple tests of {@link TraceMethod}.
 * 
 * @author Sean C. Sullivan
 * @version $Id: TestGetMethodLocal.java,v 1.3 2002/02/04 15:26:43 dion Exp $
 * 
 */
public class TestTraceMethodLocal extends TestLocalHostBase {


    // ------------------------------------------------------------ Constructor

    public TestTraceMethodLocal(String testName) {
        super(testName);
    }


    // ------------------------------------------------------- TestCase Methods


    public static Test suite() {
        return new TestSuite(TestTraceMethodLocal.class);
    }


    // ------------------------------------------------------------------ Tests



    public void testExecute() {
    	
        HttpClient client = createHttpClient();

        TraceMethod method = new TraceMethod("/");

		final String strTestHeaderName = "MyTestHeader";
		
		final String strTestHeaderValue = "This-is-a-test-value.";
		
		method.setRequestHeader(
					strTestHeaderName, 
					strTestHeaderValue);
		
        try {
            client.executeMethod(method);

			final int iResponseStatusCode = method.getStatusCode();
			assertEquals(200, iResponseStatusCode);
			
            Header[] requestHeaders = method.getRequestHeaders();
            assertTrue( requestHeaders.length > 0);

            Header[] responseHeaders = method.getResponseHeaders();
            assertNotNull(responseHeaders);
            
            //
            // note:  the reason that we convert the String's to lowercase is
            //        because some HTTP servers send a response body that contains 
            //        lower request headers
            //
            final String strResponseBody_lowercase = method.getResponseBodyAsString().toLowerCase();
            assertNotNull(strResponseBody_lowercase);
            assertTrue( strResponseBody_lowercase.length() > 0);
            
            assertTrue( strResponseBody_lowercase.indexOf(strTestHeaderName.toLowerCase()) != -1);
            assertTrue( strResponseBody_lowercase.indexOf(strTestHeaderValue.toLowerCase()) != -1);
            
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
    }
    
    public void testRecycle() {
        HttpClient client = createHttpClient();

        TraceMethod method = new TraceMethod("/");
        
        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            assertTrue("No data returned.",(data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        method.recycle();
        method.setPath("/");

        try {
            client.executeMethod(method);
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }

        try {
            String data = method.getResponseBodyAsString();
            assertTrue("No data returned.",(data.length() > 0));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Unable to execute method : " + t.toString());
        }
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestTraceMethodLocal.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }
    

}
