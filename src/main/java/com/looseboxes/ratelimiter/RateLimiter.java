package com.looseboxes.ratelimiter;

public interface RateLimiter<K> {

    RateLimiter<Object> NO_OP = (key, amount) -> { };

    @SuppressWarnings("unchecked")
    static <T> RateLimiter<T> noop() {
        return (RateLimiter<T>)NO_OP;
    }

    default void increment(K key) {
        increment(key, 1);
    }

    void increment(K key, int amount);
}
