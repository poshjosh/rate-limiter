package com.looseboxes.ratelimiter.cache;

/**
 * A cache to hold a single entry
 *
 * A {code null} key will match any/all keys.
 * @param <K> The type of the key which this Cache holds
 */
public class SingletonRateCache<K, V> implements RateCache<K, V>{

    private final K key;

    private V rate;

    public SingletonRateCache(K key) {
        this.key = key;
    }

    @Override
    public void clear() {
        this.rate = null;
    }

    @Override
    public boolean containsKey(K key) {
        return isMatchingKey(key);
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
        throw new UnsupportedOperationException("Operation not supported");
    }

    private boolean isMatchingKey(K key) {
        return this.key == null || this.key.equals(key);
    }

    private IllegalArgumentException invalidKey(K key) {
        return new IllegalArgumentException("Illegal key: " + key);
    }
}
