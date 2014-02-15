/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/test/org/apache/commons/httpclient/TestNoHost.java,v 1.39 2004/09/15 20:42:17 olegk Exp $
 * $Revision: 1.39 $
 * $Date: 2004-09-15 22:44:02 +0200 (Wed, 15 Sep 2004) $
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
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.auth.TestBasicAuth;
import org.apache.commons.httpclient.auth.TestChallengeParser;
import org.apache.commons.httpclient.auth.TestChallengeProcessor;
import org.apache.commons.httpclient.cookie.TestCookieAll;
import org.apache.commons.httpclient.params.TestHttpParams;

/**
 * Tests that don't require any external host.
 * I.e., that run entirely within this JVM.
 *
 * (True unit tests, by some definitions.)
 *
 * @author Rodney Waldhoff
 * @author <a href="mailto:jsdever@apache.org">Jeff Dever</a>
 * @version $Revision: 1.39 $ $Date: 2004-09-15 22:44:02 +0200 (Wed, 15 Sep 2004) $
 */
public class TestNoHost extends TestCase {

    public TestNoHost(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(TestHttpStatus.suite());
        suite.addTest(TestCookieAll.suite());
        suite.addTest(TestNVP.suite());
        suite.addTest(TestHeader.suite());
        suite.addTest(TestParameterParser.suite());
        suite.addTest(TestHeaderElement.suite());
        suite.addTest(TestChallengeParser.suite());
        suite.addTest(TestChallengeProcessor.suite());
        suite.addTest(TestAuthenticator.suite());
        suite.addTest(TestBasicAuth.suite());
        suite.addTest(TestRedirects.suite());
        suite.addTest(TestHttpUrlMethod.suite());
        suite.addTest(TestURI.suite());
        suite.addTest(TestURIUtil.suite());
        suite.addTest(TestURIUtil2.suite());
        suite.addTest(TestMethodsNoHost.suite());
        suite.addTest(TestMethodsRedirectNoHost.suite());
        suite.addTest(TestHttpState.suite());
        suite.addTest(TestResponseHeaders.suite());
        suite.addTest(TestRequestHeaders.suite());
        suite.addTest(TestStreams.suite());
        suite.addTest(TestStatusLine.suite());
        suite.addTest(TestRequestLine.suite());
        suite.addTest(TestPartsNoHost.suite());
        suite.addTest(TestMethodCharEncoding.suite());
        suite.addTest(TestExceptions.suite());
        suite.addTest(TestHttpVersion.suite());
        suite.addTest(TestEffectiveHttpVersion.suite());
        suite.addTest(TestHttpParser.suite());
        suite.addTest(TestBadContentLength.suite());
        suite.addTest(TestEquals.suite());
        suite.addTestSuite(TestIdleConnectionTimeout.class);
        suite.addTest(TestMethodAbort.suite());
        suite.addTest(TestHttpParams.suite());
        return suite;
    }

    public static void main(String args[]) {
        String[] testCaseName = { TestNoHost.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

}
