package com.looseboxes.ratelimiter.cache;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;

public class InMemoryRateCache<K> implements RateCache<K>{

    private final Map<K, Rate> delegate;

    public InMemoryRateCache() {
        this(new ConcurrentHashMap<>());
    }

    public InMemoryRateCache(Map<K, Rate> delegate) {
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

    @Override public <T> T unwrap(Class<T> clazz) {
        if (clazz.isAssignableFrom(delegate.getClass())) {
            return clazz.cast(delegate);
        }
        throw new IllegalArgumentException("Unwrapping to " + clazz + " is not " +
                "supported by this implementation");
    }
}
