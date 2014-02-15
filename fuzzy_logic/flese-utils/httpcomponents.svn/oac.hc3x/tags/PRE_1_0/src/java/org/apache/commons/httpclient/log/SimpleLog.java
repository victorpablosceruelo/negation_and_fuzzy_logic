/*
 * Copyright (C) The Apache Software Foundation. All rights reserved.
 *
 * This software is published under the terms of the Apache Software License
 * version 1.1, a copy of which has been included with this distribution in
 * the LICENSE file.
 */

package org.apache.commons.httpclient.log;

import java.util.Properties;
import java.util.Enumeration;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.text.DateFormat;
import java.util.Date;

/**
 * @author Rod Waldhoff
 * @version $Id: SimpleLog.java 133496 2001-08-10 17:17:39Z rwaldhoff $
 */
public class SimpleLog implements Log {
    static protected final Properties _simplelogProps = new Properties();
    static protected boolean _showlogname = false;
    static protected boolean _showtime = false;
    static protected DateFormat _df = null;

    static {
        // add all system props that start with "httpclient."
        Enumeration enum = System.getProperties().propertyNames();
        while(enum.hasMoreElements()) {
            String name = (String)(enum.nextElement());
            if(null != name && name.startsWith("httpclient.")) {
                _simplelogProps.setProperty(name,System.getProperty(name));
            }
        }

        // add props from the resource simplelog.properties
        InputStream in = ClassLoader.getSystemResourceAsStream("simplelog.properties");
        if(null != in) {
            try {
                _simplelogProps.load(in);
                in.close();
            } catch(java.io.IOException e) {
                // ignored
            }
        }
        try {
        } catch(Throwable t) {
            // ignored
        }
        _showlogname = "true".equalsIgnoreCase(_simplelogProps.getProperty("httpclient.simplelog.showlogname","true"));
        _showtime = "true".equalsIgnoreCase(_simplelogProps.getProperty("httpclient.simplelog.showdate","true"));
        if(_showtime) {
            _df = new SimpleDateFormat(_simplelogProps.getProperty("httpclient.simplelog.dateformat","yyyy/MM/dd HH:mm:ss:SSS zzz"));
        }
    }

    protected int _logLevel = Log.ERROR;

    protected String _name = null;

    public SimpleLog(String name) {
        _name = name;

        String lvl = _simplelogProps.getProperty("httpclient.simplelog.log." + _name);
        int i = String.valueOf(name).lastIndexOf(".");
        while(null == lvl && i > -1) {
            name = name.substring(0,i);
            lvl = _simplelogProps.getProperty("httpclient.simplelog.log." + name);
            i = String.valueOf(name).lastIndexOf(".");
        }
        if(null == lvl) {
            lvl =  _simplelogProps.getProperty("httpclient.simplelog.defaultlog");
        }

        if("debug".equalsIgnoreCase(lvl)) {
            _logLevel = DEBUG;
        } else if("info".equalsIgnoreCase(lvl)) {
            _logLevel = INFO;
        } else if("warn".equalsIgnoreCase(lvl)) {
            _logLevel = WARN;
        } else if("error".equalsIgnoreCase(lvl)) {
            _logLevel = ERROR;
        } else if("fatal".equalsIgnoreCase(lvl)) {
            _logLevel = FATAL;
        }
    }

    protected void log(int type, Object message, Throwable t) {
        if(_logLevel <= type) {
            StringBuffer buf = new StringBuffer();
            if(_showtime) {
                buf.append(_df.format(new Date()));
                buf.append(" ");
            }
            switch(type) {
                case DEBUG: buf.append("[DEBUG] "); break;
                case INFO:  buf.append("[INFO] ");  break;
                case WARN:  buf.append("[WARN] ");  break;
                case ERROR: buf.append("[ERROR] "); break;
                case FATAL: buf.append("[FATAL] "); break;
            }
            if(_showlogname) {
                buf.append(String.valueOf(_name)).append(" - ");
            }
            buf.append(String.valueOf(message));
            if(t != null) {
                buf.append(" <");
                buf.append(t.toString());
                buf.append(">");
                t.printStackTrace();
            }
            System.out.println(buf.toString());
        }
    }

    public final void debug(Object message) {
        log(Log.DEBUG,message,null);
    }

    public final void debug(Object message, Throwable t) {
        log(Log.DEBUG,message,t);
    }

    public final void info(Object message) {
        log(Log.INFO,message,null);
    }

    public final void info(Object message, Throwable t) {
        log(Log.INFO,message,t);
    }

    public final void warn(Object message) {
        log(Log.WARN,message,null);
    }
    public final void warn(Object message, Throwable t) {
        log(Log.WARN,message,t);
    }

    public final void error(Object message) {
        log(Log.ERROR,message,null);
    }

    public final void error(Object message, Throwable t) {
        log(Log.ERROR,message,t);
    }

    public final void fatal(Object message) {
        log(Log.FATAL,message,null);
    }

    public final void fatal(Object message, Throwable t) {
        log(Log.FATAL,message,t);
    }

    public final boolean isDebugEnabled() {
        return (_logLevel <= DEBUG);
    }

    public final boolean isInfoEnabled() {
        return (_logLevel <= INFO);
    }

    public final void setLevel(int level) {
        _logLevel = level;
    }

    public final int getLevel() {
        return _logLevel;
    }

}
