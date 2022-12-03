package com.looseboxes.ratelimiter.cache;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

class MapRateCache<K, V> implements RateCache<K, V>{

    private final Map<K, V> delegate;

    MapRateCache() {
        this(new ConcurrentHashMap<>());
    }

    MapRateCache(Map<K, V> delegate) {
        this.delegate = new ConcurrentHashMap<>(delegate);
    }

    @Override
    public void clear() {
        delegate.clear();
    }

    @Override
    public boolean containsKey(K key) {
        return delegate.containsKey(key);
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
        throw new IllegalArgumentException("Unwrapping to " + clazz + " is not supported by this implementation");
    }
}
