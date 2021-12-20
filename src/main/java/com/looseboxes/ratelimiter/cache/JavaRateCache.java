package com.looseboxes.ratelimiter.cache;

import com.looseboxes.ratelimiter.rates.Rate;

import javax.cache.Cache;
import java.util.Objects;
import java.util.function.BiConsumer;

public class JavaRateCache<K> implements RateCache<K>{

    private final Cache<K, Rate> delegate;

    public JavaRateCache(Cache<K, Rate> delegate) {
        this.delegate = Objects.requireNonNull(delegate);
    }

    @Override
    public void forEach(BiConsumer<K, Rate> consumer) {
        synchronized (delegate) {
            for (Cache.Entry<K, Rate> entry : delegate) {
                consumer.accept(entry.getKey(), entry.getValue());
            }
        }
    }

    @Override
    public Rate get(K key) {
        synchronized (delegate) {
            return delegate.get(key);
        }
    }

    @Override
    public boolean putIfAbsent(K key, Rate value) {
        synchronized (delegate) {
            return delegate.putIfAbsent(key, value);
        }
    }

    @Override
    public void put(K key, Rate value) {
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
