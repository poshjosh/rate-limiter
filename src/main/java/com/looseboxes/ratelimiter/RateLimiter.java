package com.looseboxes.ratelimiter;

public interface RateLimiter<K> {

    RateLimiter<Object> NO_OP = (resource, resourceId, amount) -> false;

    @SuppressWarnings("unchecked")
    static <T> RateLimiter<T> noop() {
        return (RateLimiter<T>)NO_OP;
    }

    /**
     * Increment the rate (identified by the specified key) by one.
     *
     * @param resourceId The id of the resource whose rate is to be incremented
     * @return {code false} if the increment caused one or more limit(s) to be exceeded, otherwise return {@code true}
     * @see #increment(Object, int)
     */
    default boolean increment(K resourceId) {
        return increment(resourceId, 1);
    }

    /**
     * Increment the rate (identified by the specified key) by the specified amount.
     *
     * @param resourceId The id of the resource whose rate is to be incremented
     * @param amount The amount by which to increment the rate
     * @return {code false} if the increment caused one or more limit(s) to be exceeded, otherwise return {@code true}
     * @see #increment(Object)
     */
    default boolean increment(K resourceId, int amount) {
        return increment(resourceId, resourceId, amount);
    }

    boolean increment(Object resource, K resourceId, int amount);
}
