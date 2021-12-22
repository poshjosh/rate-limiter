package com.looseboxes.ratelimiter.cache;

import com.looseboxes.ratelimiter.rates.Rate;

import java.io.Serializable;
import java.util.function.BiConsumer;

/**
 * A cache to hold a single entry
 *
 * A {code null} key will match any/all keys.
 * @param <K> The type of the key which this Cache holds
 */
public class SingletonRateCache<K extends Serializable, V extends Serializable> implements RateCache<K, V>{

    private final K key;

    private V rate;

    public SingletonRateCache(K key) {
        this.key = key;
    }

    @Override
    public void forEach(BiConsumer<K, V> consumer) {
        consumer.accept(key, rate);
    }

    @Override
    public V get(K key) {
        if(isMatchingKey(key)) {
            return rate;
        }else{
            throw invalidKey(key);
        }
    }

    @Override
    public boolean putIfAbsent(K key, V value) {
        if(isMatchingKey(key)) {
            if(this.rate == null) {
                this.rate = value;
                return true;
            }
            return false;
        }else{
            throw invalidKey(key);
        }
    }

    @Override
    public void put(K key, V value) {
        if(isMatchingKey(key)) {
            this.rate = value;
        }else{
            throw invalidKey(key);
        }
    }

    @Override
    public boolean remove(K key) {
        if(isMatchingKey(key)) {
            final V previous = this.rate;
            this.rate = null;
            return previous != null;
        }else{
            throw invalidKey(key);
        }
    }

    @Override public <T> T unwrap(Class<T> clazz) {
        throw new IllegalArgumentException();
    }

    private boolean isMatchingKey(K key) {
        return this.key == null || this.key.equals(key);
    }

    private IllegalArgumentException invalidKey(K key) {
        return new IllegalArgumentException("Illegal key: " + key);
    }
}
