package wip;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.cache.RateCache;

/**
 * A cache to hold a single entry
 *
 * A {code null} key will match any/all keys.
 * @param <K> The type of the key which this Cache holds
 */
class SingletonRateCache<K> implements RateCache<K> {

    private final K key;

    private Bandwidths rate;

    SingletonRateCache(K key) {
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
    public Bandwidths get(K key) {
        if(isMatchingKey(key)) {
            return rate;
        }else{
            throw invalidKey(key);
        }
    }

    @Override
    public boolean putIfAbsent(K key, Bandwidths value) {
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
    public void put(K key, Bandwidths value) {
        if(isMatchingKey(key)) {
            this.rate = value;
        }else{
            throw invalidKey(key);
        }
    }

    @Override
    public boolean remove(K key) {
        if(isMatchingKey(key)) {
            final Bandwidths previous = this.rate;
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
