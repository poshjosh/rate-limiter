package com.looseboxes.ratelimiter.cache;

import javax.cache.Cache;
import java.io.Serializable;
import java.util.Objects;
import java.util.function.BiConsumer;

public class JavaRateCache<K extends Serializable, V extends Serializable> implements RateCache<K, V>{

    private final Cache<K, V> delegate;

    public JavaRateCache(Cache<K, V> delegate) {
        this.delegate = Objects.requireNonNull(delegate);
    }

    @Override
    public void forEach(BiConsumer<K, V> consumer) {
        synchronized (delegate) {
            for (Cache.Entry<K, V> entry : delegate) {
                consumer.accept(entry.getKey(), entry.getValue());
            }
        }
    }

    @Override
    public V get(K key) {
        synchronized (delegate) {
            return delegate.get(key);
        }
    }

    @Override
    public boolean putIfAbsent(K key, V value) {
        synchronized (delegate) {
            return delegate.putIfAbsent(key, value);
        }
    }

    @Override
    public void put(K key, V value) {
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

    @Override public <T> T unwrap(Class<T> clazz) {
        if (clazz.isAssignableFrom(delegate.getClass())) {
            return clazz.cast(delegate);
        }
        throw new IllegalArgumentException("Unwrapping to " + clazz + " is not " +
                "supported by this implementation");
    }
}
