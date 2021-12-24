package com.looseboxes.ratelimiter;

public interface RateLimiter<K> {

    RateLimiter<Object> NO_OP = (key, amount) -> false;

    @SuppressWarnings("unchecked")
    static <T> RateLimiter<T> noop() {
        return (RateLimiter<T>)NO_OP;
    }

    /**
     * Increment the rate (identified by the specified key) by one.
     *
     * @param key The key whose rate is to be incremented
     * @return {code false} if the increment caused one or more limit(s) to be exceeded, otherwise return {@code true}
     * @see #increment(Object, int)
     */
    default boolean increment(K key) {
        return increment(key, 1);
    }

    /**
     * Increment the rate (identified by the specified key) by the specified amount.
     *
     * @param key The key whose rate is to be incremented
     * @param amount The amount by which to increment the rate
     * @return {code false} if the increment caused one or more limit(s) to be exceeded, otherwise return {@code true}
     * @see #increment(Object)
     */
    boolean increment(K key, int amount);
}
