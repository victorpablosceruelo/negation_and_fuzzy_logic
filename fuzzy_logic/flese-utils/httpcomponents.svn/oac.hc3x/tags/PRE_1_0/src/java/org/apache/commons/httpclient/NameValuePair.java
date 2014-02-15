/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/NameValuePair.java,v 1.3 2001/08/03 16:55:31 rwaldhoff Exp $
 * $Revision: 1.3 $
 * $Date: 2001-08-03 18:55:31 +0200 (Fri, 03 Aug 2001) $
 * ====================================================================
 * Copyright (C) The Apache Software Foundation. All rights reserved.
 *
 * This software is published under the terms of the Apache Software License
 * version 1.1, a copy of which has been included with this distribution in
 * the LICENSE file.
 */

package org.apache.commons.httpclient;

import java.io.Serializable;

/**
 * A simple class encapsulating a name/value pair.
 *
 * @author <a href="mailto:bcholmes@interlog.com">B.C. Holmes</a>
 */
public class NameValuePair implements Serializable {

    // ----------------------------------------------------------- Constructors

    /**
     * Default constructor.
     */
    public NameValuePair() {
        this(null,null);
    }

    /**
     * Constructor.
     */
    public NameValuePair(String name, String value) {

        this.name = name;
        this.value = value;

    }

    // ----------------------------------------------------- Instance Variables

    /**
     * Name.
     */
    protected String name = null;

    /**
     * Value.
     */
    protected String value = null;

    // ------------------------------------------------------------- Properties

    /**
     * Name property setter.
     *
     * @param name
     */
    public void setName(String name) {
        this.name = name;
    }


    /**
     * Name property getter.
     *
     * @return String name
     */
    public String getName() {
        return name;
    }


    /**
     * Value property setter.
     *
     * @param value
     */
    public void setValue(String value) {
        this.value = value;
    }


    /**
     * Value property getter.
     *
     * @return String value
     */
    public String getValue() {
        return value;
    }

    // --------------------------------------------------------- Public Methods

    /**
     * Get a String representation of this pair.
     */
    public String toString() {
        return ("name=" + name + ", " + "value=" + value);
    }

    /**
     * Test if the given <i>object</i> is equal to me.
     * In this implementation, an <i>object</i> is
     * equal to me iff it has the same runtime
     * type and the <i>name</i> and <i>value</i> attributes
     * are both <tt>equal</tt> (or <tt>==</tt>).
     *
     * @param object the {@link Object} to compare to
     */
    public boolean equals(Object object) {
        if (this == object) {
            return true;
        } else if (this.getClass().equals(object.getClass())) {
            NameValuePair pair = (NameValuePair) object;
            return ((null == name ? null == pair.name : name.equals(pair.name))
                   && (null == value ? null == pair.value : value.equals(pair.value)));
        } else {
            return false;
        }
    }

    /**
     * Returns a hash code for this object such that
     * if <tt>a.{@link #equals equals}(b)</tt> then
     * <tt>a.hashCode() == b.hashCode()</tt>.
     */
    public int hashCode() {
        return (this.getClass().hashCode() ^
               (null == name ? 0 : name.hashCode()) ^
               (null == value ? 0 : value.hashCode()));
    }
}
