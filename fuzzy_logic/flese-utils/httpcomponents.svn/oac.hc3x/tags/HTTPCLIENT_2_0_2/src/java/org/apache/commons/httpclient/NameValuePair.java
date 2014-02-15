/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//httpclient/src/java/org/apache/commons/httpclient/NameValuePair.java,v 1.14.2.1 2004/02/22 18:21:13 olegk Exp $
 * $Revision: 1.14.2.1 $
 * $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 *
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

import java.io.Serializable;

/**
 * <p>A simple class encapsulating a name/value pair.</p>
 * 
 * @author <a href="mailto:bcholmes@interlog.com">B.C. Holmes</a>
 * @author Sean C. Sullivan
 * @author <a href="mailto:mbowler@GargoyleSoftware.com">Mike Bowler</a>
 * 
 * @version $Revision: 1.14.2.1 $ $Date: 2004-02-22 19:21:18 +0100 (Sun, 22 Feb 2004) $
 * 
 */
public class NameValuePair implements Serializable {

    // ----------------------------------------------------------- Constructors

    /**
     * Default constructor.
     * 
     */
    public NameValuePair() {
        this (null, null);
    }

    /**
     * Constructor.
     * @param name The name.
     * @param value The value.
     */
    public NameValuePair(String name, String value) {
        this.name = name;
        this.value = value;
    }

    // ----------------------------------------------------- Instance Variables

    /**
     * Name.
     */
    private String name = null;

    /**
     * Value.
     */
    private String value = null;

    // ------------------------------------------------------------- Properties

    /**
     * Set the name.
     *
     * @param name The new name
     * @see #getName()
     */
    public void setName(String name) {
        this.name = name;
    }


    /**
     * Return the name.
     *
     * @return String name The name
     * @see #setName(String)
     */
    public String getName() {
        return name;
    }


    /**
     * Set the value.
     *
     * @param value The new value.
     */
    public void setValue(String value) {
        this.value = value;
    }


    /**
     * Return the current value.
     *
     * @return String value The current value.
     */
    public String getValue() {
        return value;
    }

    // --------------------------------------------------------- Public Methods

    /**
     * Get a String representation of this pair.
     * @return A string representation.
     */
    public String toString() {
        return ("name=" + name + ", " + "value=" + value);
    }

    /**
     * Test if the given <i>object</i> is equal to me. In this implementation,
     * an <i>object</i> is equal to me iff it has the same runtime type and the
     * <i>name</i> and <i>value</i> attributes are both <tt>equal</tt> (or
     * <tt>==</tt>).
     *
     * @param object the {@link Object} to compare to
     * @return true if the objects are equal.
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
     * hashCode. Returns a hash code for this object such that if <tt>a.{@link
     * #equals equals}(b)</tt> then <tt>a.hashCode() == b.hashCode()</tt>.
     * @return The hash code.
     */
    public int hashCode() {
        return (this.getClass().hashCode() 
            ^ (null == name ? 0 : name.hashCode()) 
            ^ (null == value ? 0 : value.hashCode()));
    }

    /*
    public Object clone() {
        try {
            NameValuePair that = (NameValuePair)(super.clone());
            that.setName(this.getName());
            that.setValue(this.getValue());
            return that;
        } catch(CloneNotSupportedException e) {
            // this should never happen
            throw new RuntimeException("Panic. super.clone not supported in NameValuePair.");
        }
    }
    */
}
