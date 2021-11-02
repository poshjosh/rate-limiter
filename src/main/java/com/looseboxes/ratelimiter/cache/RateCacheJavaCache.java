package com.looseboxes.ratelimiter.cache;

import com.looseboxes.ratelimiter.rates.Rate;

import javax.cache.Cache;
import java.util.Objects;
import java.util.function.BiConsumer;

public class RateCacheJavaCache<K> implements RateCache<K>{

    private final Cache<K, Rate> delegate;

    public RateCacheJavaCache(Cache<K, Rate> delegate) {
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
}
