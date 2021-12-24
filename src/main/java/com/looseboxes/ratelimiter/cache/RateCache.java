package com.looseboxes.ratelimiter.cache;

public interface RateCache<K, V> {

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
