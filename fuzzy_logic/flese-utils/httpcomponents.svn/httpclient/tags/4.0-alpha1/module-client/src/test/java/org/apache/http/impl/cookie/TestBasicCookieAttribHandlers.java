/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha1/module-client/src/test/java/org/apache/http/impl/cookie/TestBasicCookieAttribHandlers.java $
 * $Revision: 548073 $
 * $Date: 2007-06-17 19:02:01 +0200 (Sun, 17 Jun 2007) $
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.http.impl.cookie;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.http.cookie.Cookie;
import org.apache.http.cookie.CookieAttributeHandler;
import org.apache.http.cookie.CookieOrigin;
import org.apache.http.cookie.MalformedCookieException;


public class TestBasicCookieAttribHandlers extends TestCase {

    public TestBasicCookieAttribHandlers(String testName) {
        super(testName);
    }

    public static Test suite() {
        return new TestSuite(TestBasicCookieAttribHandlers.class);
    }

    // ------------------------------------------------------------------- Main
    public static void main(String args[]) {
        String[] testCaseName = { TestBasicCookieAttribHandlers.class.getName() };
        junit.textui.TestRunner.main(testCaseName);
    }

    public void testBasicDomainParse() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieAttributeHandler h = new BasicDomainHandler();
        h.parse(cookie, "www.somedomain.com");
        assertEquals("www.somedomain.com", cookie.getDomain());
        assertTrue(cookie.isDomainAttributeSpecified());
    }

    public void testBasicDomainParseInvalid() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieAttributeHandler h = new BasicDomainHandler();
        try {
            h.parse(cookie, "");
            fail("MalformedCookieException should have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
        try {
            h.parse(cookie, null);
            fail("MalformedCookieException should have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
    }

    public void testBasicDomainValidate1() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieOrigin origin = new CookieOrigin("www.somedomain.com", 80, "/", false); 
        CookieAttributeHandler h = new BasicDomainHandler();
        
        cookie.setDomain(".somedomain.com");
        h.validate(cookie, origin);

        cookie.setDomain(".otherdomain.com");
        try {
            h.validate(cookie, origin);
            fail("MalformedCookieException should have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
        cookie.setDomain("www.otherdomain.com");
        try {
            h.validate(cookie, origin);
            fail("MalformedCookieException should have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
    }

    public void testBasicDomainValidate2() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieOrigin origin = new CookieOrigin("somehost", 80, "/", false); 
        CookieAttributeHandler h = new BasicDomainHandler();
        
        cookie.setDomain("somehost");
        h.validate(cookie, origin);

        cookie.setDomain("otherhost");
        try {
            h.validate(cookie, origin);
            fail("MalformedCookieException should have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
    }

    public void testBasicDomainValidate3() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieOrigin origin = new CookieOrigin("somedomain.com", 80, "/", false); 
        CookieAttributeHandler h = new BasicDomainHandler();
        
        cookie.setDomain(".somedomain.com");
        h.validate(cookie, origin);
    }

    public void testBasicDomainValidate4() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieOrigin origin = new CookieOrigin("somedomain.com", 80, "/", false); 
        CookieAttributeHandler h = new BasicDomainHandler();
        
        cookie.setDomain(null);
        try {
            h.validate(cookie, origin);
            fail("MalformedCookieException should have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
    }
    
    public void testBasicDomainMatch1() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieOrigin origin = new CookieOrigin("somedomain.com", 80, "/", false); 
        CookieAttributeHandler h = new BasicDomainHandler();

        cookie.setDomain("somedomain.com");
        assertTrue(h.match(cookie, origin));
        
        cookie.setDomain(".somedomain.com");
        assertTrue(h.match(cookie, origin));
    }

    public void testBasicDomainMatch2() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieOrigin origin = new CookieOrigin("www.somedomain.com", 80, "/", false); 
        CookieAttributeHandler h = new BasicDomainHandler();

        cookie.setDomain("somedomain.com");
        assertTrue(h.match(cookie, origin));
        
        cookie.setDomain(".somedomain.com");
        assertTrue(h.match(cookie, origin));

        cookie.setDomain(null);
        assertFalse(h.match(cookie, origin));
    }

    public void testBasicDomainInvalidInput() throws Exception {
        CookieAttributeHandler h = new BasicDomainHandler();
        try {
            h.parse(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            h.validate(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            h.validate(new Cookie("name", "value"), null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            h.match(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            h.match(new Cookie("name", "value"), null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }

    public void testBasicPathParse() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieAttributeHandler h = new BasicPathHandler();
        h.parse(cookie, "stuff");
        assertEquals("stuff", cookie.getPath());
        assertTrue(cookie.isPathAttributeSpecified());
        h.parse(cookie, "");
        assertEquals("/", cookie.getPath());
        assertTrue(cookie.isPathAttributeSpecified());
        h.parse(cookie, null);
        assertEquals("/", cookie.getPath());
        assertTrue(cookie.isPathAttributeSpecified());
    }

    public void testBasicPathMatch1() throws Exception {
        Cookie cookie = new Cookie("name", "value");
        CookieOrigin origin = new CookieOrigin("somehost", 80, "/stuff", false); 
        CookieAttributeHandler h = new BasicPathHandler();
        cookie.setPath("/stuff");
        assertTrue(h.match(cookie, origin));
    }
    
    public void testBasicPathMatch2() throws Exception {
        Cookie cookie = new Cookie("name", "value");
        CookieOrigin origin = new CookieOrigin("somehost", 80, "/stuff/", false); 
        CookieAttributeHandler h = new BasicPathHandler();
        cookie.setPath("/stuff");
        assertTrue(h.match(cookie, origin));
    }
    
    public void testBasicPathMatch3() throws Exception {
        Cookie cookie = new Cookie("name", "value");
        CookieOrigin origin = new CookieOrigin("somehost", 80, "/stuff/more-stuff", false); 
        CookieAttributeHandler h = new BasicPathHandler();
        cookie.setPath("/stuff");
        assertTrue(h.match(cookie, origin));
    }
    
    public void testBasicPathMatch4() throws Exception {
        Cookie cookie = new Cookie("name", "value");
        CookieOrigin origin = new CookieOrigin("somehost", 80, "/stuffed", false); 
        CookieAttributeHandler h = new BasicPathHandler();
        cookie.setPath("/stuff");
        assertFalse(h.match(cookie, origin));
    }

    public void testBasicPathMatch5() throws Exception {
        Cookie cookie = new Cookie("name", "value");
        CookieOrigin origin = new CookieOrigin("somehost", 80, "/otherstuff", false); 
        CookieAttributeHandler h = new BasicPathHandler();
        cookie.setPath("/stuff");
        assertFalse(h.match(cookie, origin));
    }

    public void testBasicPathMatch6() throws Exception {
        Cookie cookie = new Cookie("name", "value");
        CookieOrigin origin = new CookieOrigin("somehost", 80, "/stuff", false); 
        CookieAttributeHandler h = new BasicPathHandler();
        cookie.setPath("/stuff/");
        assertTrue(h.match(cookie, origin));
    }

    public void testBasicPathMatch7() throws Exception {
        Cookie cookie = new Cookie("name", "value");
        CookieOrigin origin = new CookieOrigin("somehost", 80, "/stuff", false); 
        CookieAttributeHandler h = new BasicPathHandler();
        assertTrue(h.match(cookie, origin));
    }

    public void testBasicPathValidate() throws Exception {
        Cookie cookie = new Cookie("name", "value");
        CookieOrigin origin = new CookieOrigin("somehost", 80, "/stuff", false); 
        CookieAttributeHandler h = new BasicPathHandler();
        cookie.setPath("/stuff");
        h.validate(cookie, origin);
        cookie.setPath("/stuffed");
        try {
            h.validate(cookie, origin);
            fail("MalformedCookieException must have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
    }

    public void testBasicPathInvalidInput() throws Exception {
        CookieAttributeHandler h = new BasicPathHandler();
        try {
            h.parse(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            h.match(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            h.match(new Cookie("name", "value"), null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }

    public void testBasicMaxAgeParse() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieAttributeHandler h = new BasicMaxAgeHandler();
        h.parse(cookie, "2000");
        assertNotNull(cookie.getExpiryDate());
    }

    public void testBasicMaxAgeParseInvalid() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieAttributeHandler h = new BasicMaxAgeHandler();
        try {
            h.parse(cookie, "garbage");
            fail("MalformedCookieException must have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
        try {
            h.parse(cookie, null);
            fail("MalformedCookieException must have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
    }

    public void testBasicMaxAgeInvalidInput() throws Exception {
        CookieAttributeHandler h = new BasicMaxAgeHandler();
        try {
            h.parse(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }

    public void testBasicCommentParse() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieAttributeHandler h = new BasicCommentHandler();
        h.parse(cookie, "whatever");
        assertEquals("whatever", cookie.getComment());
        h.parse(cookie, null);
        assertEquals(null, cookie.getComment());
    }

    public void testBasicCommentInvalidInput() throws Exception {
        CookieAttributeHandler h = new BasicCommentHandler();
        try {
            h.parse(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }
    
    public void testBasicSecureParse() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieAttributeHandler h = new BasicSecureHandler();
        h.parse(cookie, "whatever");
        assertTrue(cookie.isSecure());
        h.parse(cookie, null);
        assertTrue(cookie.isSecure());
    }

    public void testBasicSecureMatch() throws Exception {
        Cookie cookie = new Cookie("name", "value");
        CookieAttributeHandler h = new BasicSecureHandler();

        CookieOrigin origin1 = new CookieOrigin("somehost", 80, "/stuff", false); 
        cookie.setSecure(false);
        assertTrue(h.match(cookie, origin1));
        cookie.setSecure(true);
        assertFalse(h.match(cookie, origin1));

        CookieOrigin origin2 = new CookieOrigin("somehost", 80, "/stuff", true); 
        cookie.setSecure(false);
        assertTrue(h.match(cookie, origin2));
        cookie.setSecure(true);
        assertTrue(h.match(cookie, origin2));
    }

    public void testBasicSecureInvalidInput() throws Exception {
        CookieAttributeHandler h = new BasicSecureHandler();
        try {
            h.parse(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            h.match(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        try {
            h.match(new Cookie("name", "value"), null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }

    public void testBasicExpiresParse() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieAttributeHandler h = new BasicExpiresHandler(new String[] {DateUtils.PATTERN_RFC1123});
        
        DateFormat dateformat = new SimpleDateFormat(DateUtils.PATTERN_RFC1123, Locale.US);
        dateformat.setTimeZone(DateUtils.GMT);
        
        Date now = new Date();
        
        h.parse(cookie, dateformat.format(now));
        assertNotNull(cookie.getExpiryDate());
    }
    
    public void testBasicExpiresParseInvalid() throws Exception {
        Cookie cookie = new Cookie("name", "value"); 
        CookieAttributeHandler h = new BasicExpiresHandler(new String[] {DateUtils.PATTERN_RFC1123});
        try {
            h.parse(cookie, "garbage");
            fail("MalformedCookieException must have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
        try {
            h.parse(cookie, null);
            fail("MalformedCookieException must have been thrown");
        } catch (MalformedCookieException ex) {
            // expected
        }
    }

    public void testBasicExpiresInvalidInput() throws Exception {
        try {
            new BasicExpiresHandler(null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
        CookieAttributeHandler h = new BasicExpiresHandler(new String[] {DateUtils.PATTERN_RFC1123});
        try {
            h.parse(null, null);
            fail("IllegalArgumentException must have been thrown");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }
    
}
