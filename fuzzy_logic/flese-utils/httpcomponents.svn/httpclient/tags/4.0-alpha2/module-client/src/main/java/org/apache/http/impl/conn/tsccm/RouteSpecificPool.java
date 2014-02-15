/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpclient/tags/4.0-alpha2/module-client/src/main/java/org/apache/http/impl/conn/tsccm/RouteSpecificPool.java $
 * $Revision: 564938 $
 * $Date: 2007-08-11 18:42:58 +0200 (Sat, 11 Aug 2007) $
 *
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

package org.apache.http.impl.conn.tsccm;

import java.util.LinkedList;

import org.apache.http.conn.HttpRoute;

/**
 * A connection sub-pool for a specific route, used by {@link ConnPoolByRoute}.
 * The methods in this class are unsynchronized. It is expected that the
 * containing pool takes care of synchronization.
 */
public class RouteSpecificPool {

    //@@@ change attribute visibility to protected once it is ensured
    //@@@ that there is no direct attribute access within this package

    /** The route this pool is for. */
    private final HttpRoute route;

    /** The list of free entries. */
    private LinkedList freeEntries;

    /** The list of threads waiting for this pool. */
    /*private@@@ currently still default*/ LinkedList waitingThreads;

    /** The number of created entries. */
    private int numEntries;


    /**
     * Creates a new route-specific pool.
     *
     * @param r     the route for which to pool
         */
    public RouteSpecificPool(HttpRoute r) {
        this.route = r;
        this.freeEntries = new LinkedList();
        this.waitingThreads = new LinkedList();
        this.numEntries = 0;
    }


    /**
     * Obtains the route for which this pool is specific.
     *
     * @return  the route
     */
    public final HttpRoute getRoute() {
        return route;
    }


    /**
     * Indicates whether this pool is unused.
     * A pool is unused if there is neither an entry nor a waiting thread.
     * All entries count, not only the free but also the allocated ones.
     *
     * @return  <code>true</code> if this pool is unused,
     *          <code>false</code> otherwise
     */
    public boolean isUnused() {
        return (numEntries < 1) && waitingThreads.isEmpty();
    }


    /**
     * Obtains the number of entries.
     * This includes not only the free entries, but also those that
     * have been created and are currently issued to an application.
     *
     * @return  the number of entries for the route of this pool
     */
    public final int getEntryCount() {
        return numEntries;
    }


    /**
     * Obtains a free entry from this pool, if one is available.
     *
     * @return an available pool entry, or <code>null</code> if there is none
     */
    public BasicPoolEntry allocEntry() {

        BasicPoolEntry entry = null;

        if (!freeEntries.isEmpty()) {
            entry = (BasicPoolEntry) freeEntries.removeLast();
        }

        return entry;
    }


    /**
     * Returns an allocated entry to this pool.
     *
     * @param entry     the entry obtained from {@link #allocEntry allocEntry}
     *                  or presented to {@link #createdEntry createdEntry}
     */
    public void freeEntry(BasicPoolEntry entry) {

        if (numEntries < 1) {
            throw new IllegalStateException
                ("No entry created for this pool. " + route);
        }
        if (numEntries <= freeEntries.size()) {
            throw new IllegalStateException
                ("No entry allocated from this pool. " + route);
        }
        freeEntries.add(entry);
    }


    /**
     * Indicates creation of an entry for this pool.
     * The entry will <i>not</i> be added to the list of free entries,
     * it is only recognized as belonging to this pool now. It can then
     * be passed to {@link #freeEntry freeEntry}.
     *
     * @param entry     the entry that was created for this pool
     */
    public void createdEntry(BasicPoolEntry entry) {

        if (!route.equals(entry.getPlannedRoute())) {
            throw new IllegalArgumentException
                ("Entry not planned for this pool." +
                 "\npool: " + route +
                 "\nplan: " + entry.getPlannedRoute());
        }

        numEntries++;
    }


    /**
     * Deletes an entry from this pool.
     * Only entries that are currently free in this pool can be deleted.
     * Allocated entries can not be deleted.
     *
     * @param entry     the entry to delete from this pool
     *
     * @return  <code>true</code> if the entry was found and deleted, or
     *          <code>false</code> if the entry was not found
     */
    public boolean deleteEntry(BasicPoolEntry entry) {

        final boolean found = freeEntries.remove(entry);
        if (found)
            numEntries--;
        return found;
    }


    /**
     * Forgets about an entry from this pool.
     * This method is used to indicate that an entry
     * {@link #allocEntry allocated}
     * from this pool has been lost and will not be returned.
     */
    public void dropEntry() {
        if (numEntries < 1) {
            throw new IllegalStateException
                ("There is no entry that could be dropped.");
        }
        numEntries--;
    }


} // class RouteSpecificPool
