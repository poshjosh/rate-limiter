package com.looseboxes.ratelimiter.cache;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;

import javax.cache.Cache;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public interface RateCache<K> {

    static <K> RateCache<K> of(Cache<K, Bandwidths> cache) {
        return new JavaRateCache<>(cache);
    }

    static <K> RateCache<K> of(Map<K, Bandwidths> map) {
        return new MapRateCache<>(map);
    }

    static <K> RateCache<K> ofMap() {
        return of(new ConcurrentHashMap<>());
    }

    void clear();

    boolean containsKey(K key);

    Bandwidths get(K key);

    boolean putIfAbsent(K key, Bandwidths value);

    void put(K key, Bandwidths value);

    boolean remove(K key);

    /**
     * Provides a standard way to access the underlying concrete caching implementation to provide
     * access to further, proprietary features. If the specified class is not supported,
     * IllegalArgumentException is thrown.
     *
     * @param clazz – the proprietary class * or interface of the underlying concrete cache. It is
     *     this type that is returned.
     * @param <T> – the type of the underlying Cache * implementation
     * @return an instance of the underlying concrete cache
     * @throws IllegalArgumentException – if the specified class is not supported
     * @throws SecurityException – when the operation could not be performed due to the current
     *     security settings
     */
    <T> T unwrap(Class<T> clazz);
}
