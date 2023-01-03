package com.looseboxes.ratelimiter.cache;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;

import javax.cache.Cache;
import java.util.Objects;

class JavaRateCache<K> implements RateCache<K>{

    private final Cache<K, Bandwidths> delegate;

    JavaRateCache(Cache<K, Bandwidths> delegate) {
        this.delegate = Objects.requireNonNull(delegate);
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
        synchronized (delegate) {
            return delegate.get(key);
        }
    }

    @Override
    public boolean putIfAbsent(K key, Bandwidths value) {
        synchronized (delegate) {
            return delegate.putIfAbsent(key, value);
        }
    }

    @Override
    public void put(K key, Bandwidths value) {
        synchronized (delegate) {
            delegate.put(key, value);
        }
    }

    @Override
    public boolean remove(K key) {
        synchronized (delegate) {
            return delegate.remove(key);
        }
    }

    @Override
    public <T> T unwrap(Class<T> clazz) {
        if (clazz.isAssignableFrom(delegate.getClass())) {
            return clazz.cast(delegate);
        }
        throw new IllegalArgumentException("Unwrapping to " + clazz + " is not supported by this implementation");
    }
}
