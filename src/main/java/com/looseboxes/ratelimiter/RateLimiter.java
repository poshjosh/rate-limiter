package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Limit;
import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Objects;

public interface RateLimiter<K> {

    RateLimiter<Object> NO_OP = (resource, resourceId, amount) -> false;

    @SuppressWarnings("unchecked")
    static <T> RateLimiter<T> noop() {
        return (RateLimiter<T>)NO_OP;
    }

    static <K> RateLimiter<K> of(Rate... rates) {
        return of(Limit.of(rates));
    }

    static <K> RateLimiter<K> of(Limit limit) {
        return of(RateLimiterConfig.newInstance(), limit);
    }

    static <K> RateLimiter<K> of(RateLimiterConfig<K, ?> rateLimiterConfig, Limit limit) {
        return new SimpleRateLimiter<>(rateLimiterConfig, limit);
    }

    /**
     * Consume the identified resource by one. Same as calling {@code #consume(k, 1)}
     *
     * @param resourceId The id of the resource whose rate is to be incremented
     * @return {code false} if the consume caused one or more limit(s) to be exceeded, otherwise return {@code true}
     * @see #consume(Object, int)
     */
    default boolean consume(K resourceId) {
        return consume(resourceId, 1);
    }

    /**
     * Consume the identified resource by the specified amount.
     *
     * @param resourceId The id of the resource whose rate is to be incremented
     * @param amount The amount by which to consume the rate
     * @return {code false} if the consume caused one or more limit(s) to be exceeded, otherwise return {@code true}
     * @see #consume(Object)
     */
    default boolean consume(K resourceId, int amount) {
        return consume(resourceId, resourceId, amount);
    }

    boolean consume(Object context, K resourceId, int amount);

    /**
     * Returns a composed RateLimiter that first calls this RateLimiter's consume function,
     * and then calls the consume function of the {@code after} RateLimiter.
     * If evaluation of either consume function throws an exception, it is relayed to
     * the caller of the composed function. Best effort is made to call both consume functions
     * before relaying the thrown exception.
     *
     * @param after The RateLimiter to consume after this RateLimiter is incremented
     * @return a composed RateLimiter that first increments this and then the {@code after} RateLimiter
     * @throws NullPointerException if after is null
     */
    default RateLimiter<K> andThen(RateLimiter<K> after) {
        Objects.requireNonNull(after);
        return (resource, resourceId, amount) -> {
            boolean a;
            boolean b;
            try {
                a = consume(resource, resourceId, amount);
            } finally {
                b = after.consume(resource, resourceId, amount);
            }
            return a && b;
        };
    }
}
