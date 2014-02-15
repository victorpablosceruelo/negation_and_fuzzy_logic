package org.apache.commons.httpclient;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.httpclient.protocol.DefaultProtocolSocketFactory;
import org.apache.commons.httpclient.protocol.Protocol;
import org.apache.commons.httpclient.protocol.ProtocolSocketFactory;
import org.apache.commons.httpclient.protocol.SSLProtocolSocketFactory;

/**
 */
public class TestEquals extends TestCase {
    
    public static Test suite() {
        return new TestSuite(TestEquals.class);
    }
    
    /**
     * 
     */
    public TestEquals() {
        super();
    }

    /**
     * @param arg0
     */
    public TestEquals(String arg0) {
        super(arg0);
    }

    public void testProtocol() {
        
        Protocol p1 = new Protocol("test", new DefaultProtocolSocketFactory(), 123);
        Protocol p2 = new Protocol("test", new DefaultProtocolSocketFactory(), 123);
        
        assertTrue(p1.equals(p2));
        assertTrue(p2.equals(p1));
    }
    
    public void testProtocolSocketFactory() {
        
        ProtocolSocketFactory p1 = new DefaultProtocolSocketFactory();
        ProtocolSocketFactory p2 = new DefaultProtocolSocketFactory();

        assertTrue(p1.equals(p2));
        assertTrue(p2.equals(p1));

        p1 = new SSLProtocolSocketFactory();
        p2 = new SSLProtocolSocketFactory();

        assertTrue(p1.equals(p2));
        assertTrue(p2.equals(p1));
        
    }
    
    public void testHostConfiguration() {
        
        HostConfiguration hc1 = new HostConfiguration();
        hc1.setHost("http", 80, "http");

        HostConfiguration hc2 = new HostConfiguration();
        hc2.setHost("http", 80, "http");

        assertTrue(hc1.equals(hc2));
        assertTrue(hc2.equals(hc1));
    }
    
}
