package com.looseboxes.ratelimiter.cache;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;

public class MapRateCache<K, V> implements RateCache<K, V>{

    private final Map<K, V> delegate;

    public MapRateCache() {
        this(new ConcurrentHashMap<>());
    }

    public MapRateCache(Map<K, V> delegate) {
        this.delegate = new ConcurrentHashMap<>(delegate);
    }

    @Override
    public void forEach(BiConsumer<K, V> consumer) {
        delegate.forEach(consumer);
    }

    @Override
    public V get(K key) {
        return delegate.get(key);
    }

    @Override
    public boolean putIfAbsent(K key, V value) {
        return delegate.putIfAbsent(key, value) == null;
    }

    @Override
    public void put(K key, V value) {
        delegate.put(key, value);
    }

    @Override
    public boolean remove(K key) {
        V result = delegate.remove(key);
        return result != null;
    }

    @Override public <T> T unwrap(Class<T> clazz) {
        if (clazz.isAssignableFrom(delegate.getClass())) {
            return clazz.cast(delegate);
        }
        throw new IllegalArgumentException("Unwrapping to " + clazz + " is not " +
                "supported by this implementation");
    }
}
