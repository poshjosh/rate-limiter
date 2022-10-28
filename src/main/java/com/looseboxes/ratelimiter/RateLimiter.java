package com.looseboxes.ratelimiter;

import java.util.Objects;

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

    /**
     * Returns a composed RateLimiter that first calls this RateLimiter's increment function,
     * and then calls the increment function of the {@code after} RateLimiter.
     * If evaluation of either increment function throws an exception, it is relayed to
     * the caller of the composed function. Best effort is made to call both increment functions
     * before relaying the thrown exception.
     *
     * @param after The RateLimiter to increment after this RateLimiter is incremented
     * @return a composed RateLimiter that first increments this and then the {@code after} RateLimiter
     * @throws NullPointerException if after is null
     */
    default RateLimiter<K> andThen(RateLimiter<K> after) {
        Objects.requireNonNull(after);
        return (resource, resourceId, amount) -> {
            boolean a;
            boolean b;
            try {
                a = increment(resource, resourceId, amount);
            } finally {
                b = after.increment(resource, resourceId, amount);
            }
            return a && b;
        };
    }
}
