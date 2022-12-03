package com.looseboxes.ratelimiter.cache;

import javax.cache.Cache;
import java.util.Map;

public interface RateCache<K, V> {

    static <K, V> RateCache<K, V> of(Cache cache) {
        return new JavaRateCache<K, V>(cache);
    }

    static <K, V> RateCache<K, V> of(Map<K, V> map) {
        return new MapRateCache<>(map);
    }

    static <K, V> RateCache<K, V> ofMap() {
        return new MapRateCache<>();
    }

    static <K, V> RateCache<K, V> singleton() {
        return singleton(null);
    }

    static <K, V> RateCache<K, V> singleton(K key) {
        return new SingletonRateCache<>(key);
    }

    void clear();

    boolean containsKey(K key);

    V get(K key);

    boolean putIfAbsent(K key, V value);

    void put(K key, V value);

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
