package io.github.poshjosh.ratelimiter.cache;

import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

class MapRateCache<K> implements RateCache<K>{

    private final Map<K, Bandwidths> delegate;

    MapRateCache(Map<K, Bandwidths> delegate) {
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
    public Bandwidths get(K key) {
        return delegate.get(key);
    }

    @Override
    public boolean putIfAbsent(K key, Bandwidths value) {
        return delegate.putIfAbsent(key, value) == null;
    }

    @Override
    public void put(K key, Bandwidths value) {
        delegate.put(key, value);
    }

    @Override
    public boolean remove(K key) {
        Bandwidths result = delegate.remove(key);
        return result != null;
    }

    @Override public <T> T unwrap(Class<T> clazz) {
        if (clazz.isAssignableFrom(delegate.getClass())) {
            return clazz.cast(delegate);
        }
        throw new IllegalArgumentException("Unwrapping to " + clazz +
                " is not supported by this implementation");
    }
}
