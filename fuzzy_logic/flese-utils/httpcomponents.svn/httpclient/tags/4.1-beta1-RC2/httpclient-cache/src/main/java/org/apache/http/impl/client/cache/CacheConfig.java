/*
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
package org.apache.http.impl.client.cache;

/**
 * Java Beans-style configuration for a
 * {@link org.apache.http.impl.client.cache.CachingHttpClient}.
 */
public class CacheConfig {

    /** Default setting for the maximum object size that will be
     * cached, in bytes.
     */
    public final static int DEFAULT_MAX_OBJECT_SIZE_BYTES = 8192;

    /** Default setting for the maximum number of cache entries
     * that will be retained.
     */
    public final static int DEFAULT_MAX_CACHE_ENTRIES = 1000;

    /** Default setting for the number of retries on a failed
     * cache update
     */
    public final static int DEFAULT_MAX_UPDATE_RETRIES = 1;

    /** Default setting for heuristic caching
     */
    public final static boolean DEFAULT_HEURISTIC_CACHING_ENABLED = false;

    /** Default coefficient used to heuristically determine freshness lifetime from
     * cache entry.
     */
    public final static float DEFAULT_HEURISTIC_COEFFICIENT = 0.1f;

    /** Default lifetime to be assumed when we cannot calculate freshness heuristically
     */
    public final static long DEFAULT_HEURISTIC_LIFETIME = 0;

    private int maxObjectSizeBytes = DEFAULT_MAX_OBJECT_SIZE_BYTES;
    private int maxCacheEntries = DEFAULT_MAX_CACHE_ENTRIES;
    private int maxUpdateRetries = DEFAULT_MAX_UPDATE_RETRIES;
    private boolean heuristicCachingEnabled = false;
    private float heuristicCoefficient = DEFAULT_HEURISTIC_COEFFICIENT;
    private long heuristicDefaultLifetime = DEFAULT_HEURISTIC_LIFETIME;
    private boolean isSharedCache = true;

    /**
     * Returns the current maximum object size that will be cached.
     * @return size in bytes
     */
    public int getMaxObjectSizeBytes() {
        return maxObjectSizeBytes;
    }

    /**
     * Specifies the maximum object size that will be eligible for caching.
     * @param maxObjectSizeBytes size in bytes
     */
    public void setMaxObjectSizeBytes(int maxObjectSizeBytes) {
        this.maxObjectSizeBytes = maxObjectSizeBytes;
    }

    /**
     * Returns whether the cache will behave as a shared cache or not.
     * @return true for a shared cache, false for a non-shared (private)
     * cache
     */
    public boolean isSharedCache() {
        return isSharedCache;
    }

    /**
     * Sets whether the cache should behave as a shared cache or not.
     * @param isSharedCache true to behave as a shared cache, false to
     * behave as a non-shared (private) cache.
     */
    public void setSharedCache(boolean isSharedCache) {
        this.isSharedCache = isSharedCache;
    }

    /**
     * Returns the maximum number of cache entries the cache will retain.
     */
    public int getMaxCacheEntries() {
        return maxCacheEntries;
    }

    /**
     * Sets the maximum number of cache entries the cache will retain.
     */
    public void setMaxCacheEntries(int maxCacheEntries) {
        this.maxCacheEntries = maxCacheEntries;
    }

    /**
     * Returns the number of times to retry a cache update on failure
     */
    public int getMaxUpdateRetries(){
        return maxUpdateRetries;
    }

    /**
     * Sets the number of times to retry a cache update on failure
     */
    public void setMaxUpdateRetries(int maxUpdateRetries){
        this.maxUpdateRetries = maxUpdateRetries;
    }

    /**
     * Returns if heuristic freshness caching is in enabled
     */
    public boolean isHeuristicCachingEnabled() {
        return heuristicCachingEnabled;
    }

    /**
     * Set if heuristic freshness caching is enabled
     */
    public void setHeuristicCachingEnabled(boolean heuristicCachingEnabled) {
        this.heuristicCachingEnabled = heuristicCachingEnabled;
    }

    /**
     * Returns coefficient used in heuristic freshness caching
     */
    public float getHeuristicCoefficient() {
        return heuristicCoefficient;
    }

    /**
     * Set coefficient to be used in heuristic freshness caching
     */
    public void setHeuristicCoefficient(float heuristicCoefficient) {
        this.heuristicCoefficient = heuristicCoefficient;
    }

    /**
     * Get the default lifetime to be used if heuristic freshness calculation is
     * not possible
     */
    public long getHeuristicDefaultLifetime() {
        return heuristicDefaultLifetime;
    }

    /**
     * Set default lifetime to be used if heuristic freshness calculation is not possible
     */
    public void setHeuristicDefaultLifetime(long heuristicDefaultLifetime) {
        this.heuristicDefaultLifetime = heuristicDefaultLifetime;
    }


}
