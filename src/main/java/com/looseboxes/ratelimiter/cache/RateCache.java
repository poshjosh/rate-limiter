package com.looseboxes.ratelimiter.cache;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.function.BiConsumer;

public interface RateCache<K> {

    void forEach(BiConsumer<K, Rate> consumer);

    Rate get(K key);

    void put(K key, Rate value);

    boolean remove(K key);
}
