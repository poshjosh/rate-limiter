package com.looseboxes.ratelimiter.cache;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;

public class RateCacheInMemory<K> implements RateCache<K>{

    private final Map<K, Rate> delegate;

    public RateCacheInMemory() {
        this(new ConcurrentHashMap<>());
    }

    public RateCacheInMemory(Map<K, Rate> delegate) {
        this.delegate = Objects.requireNonNull(delegate);
    }

    @Override
    public void forEach(BiConsumer<K, Rate> consumer) {
        delegate.forEach(consumer);
    }

    @Override
    public Rate get(K key) {
        return delegate.get(key);
    }

    @Override
    public void put(K key, Rate value) {
        delegate.put(key, value);
    }

    @Override
    public boolean remove(K key) {
        Rate result = delegate.remove(key);
        return result != null;
    }
}
