package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.Rate;
import com.looseboxes.ratelimiter.util.Rates;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MICROSECONDS;

public interface RateLimiter<K> {

    RateLimiter<Object> NO_OP = (context, resourceId, permits, timeout, unit) -> false;

    @SuppressWarnings("unchecked")
    static <T> RateLimiter<T> noop() {
        return (RateLimiter<T>)NO_OP;
    }

    static <K> RateLimiter<K> of(Rate... limits) {
        return of(Rates.of(limits));
    }

    static <K> RateLimiter<K> of(Rates limits) {
        return of(RateLimiterConfig.of(), limits);
    }

    static <K> RateLimiter<K> of(RateLimiterConfig<K, ?> rateLimiterConfig, Rate limit) {
        return of(rateLimiterConfig, Rates.of(limit));
    }

    static <K> RateLimiter<K> of(RateLimiterConfig<K, ?> rateLimiterConfig, Rates limits) {
        return bursty(rateLimiterConfig, limits);
    }

    static <K> RateLimiter<K> bursty(Rates limits) {
        return bursty(RateLimiterConfig.of(), limits);
    }

    static <K> RateLimiter<K> bursty(RateLimiterConfig<K, ?> rateLimiterConfig, Rates limits) {
        return new DefaultRateLimiter(rateLimiterConfig, limits);
    }

    static <K> RateLimiter<K> warmingUp(Rates limits) {
        return warmingUp(RateLimiterConfig.of(), limits);
    }

    static <K> RateLimiter<K> warmingUp(RateLimiterConfig<K, ?> rateLimiterConfig, Rates limits) {
        return new DefaultRateLimiter(rateLimiterConfig, limits);
    }

    /**
     * Consume the identified resource by one. Same as calling {@code #tryConsume(k, 1)}
     *
     * @param resourceId The id of the resource whose rate is to be incremented
     * @return {code false} if the tryConsume caused one or more limit(s) to be exceeded, otherwise return {@code true}
     * @see #tryConsume(Object, int)
     */
    default boolean tryConsume(K resourceId) {
        return tryConsume(resourceId, 1);
    }

    /**
     * Consume the identified resource by the specified amount.
     *
     * @param resourceId The id of the resource whose rate is to be incremented
     * @param amount The amount by which to tryConsume the rate
     * @return {code false} if the tryConsume caused one or more limit(s) to be exceeded, otherwise return {@code true}
     * @see #tryConsume(Object)
     */
    default boolean tryConsume(K resourceId, int amount) {
        return tryConsume(resourceId, resourceId, amount);
    }

    /**
     * Acquires a permit from this {@code RateLimiter} if it can be obtained without exceeding the
     * specified {@code timeout}, or returns {@code false} immediately (without waiting) if the permit
     * would not have been granted before the timeout expired.
     *
     * <p>This method is equivalent to {@code tryAcquire(1, timeout)}.
     *
     * @param timeout the maximum time to wait for the permit. Negative values are treated as zero.
     * @return {@code true} if the permit was acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     * @since 28.0
     */
    default boolean tryConsume(Object context, K resourceId, Duration timeout) {
        return tryConsume(context, resourceId, 1, Util.toNanosSaturated(timeout), TimeUnit.NANOSECONDS);
    }

    /**
     * Acquires a permit from this {@code RateLimiter} if it can be obtained without exceeding the
     * specified {@code timeout}, or returns {@code false} immediately (without waiting) if the permit
     * would not have been granted before the timeout expired.
     *
     * <p>This method is equivalent to {@code tryAcquire(1, timeout, unit)}.
     *
     * @param timeout the maximum time to wait for the permit. Negative values are treated as zero.
     * @param unit the time unit of the timeout argument
     * @return {@code true} if the permit was acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    @SuppressWarnings("GoodTime") // should accept a java.time.Duration
    default boolean tryConsume(Object context, K resourceId, long timeout, TimeUnit unit) {
        return tryConsume(context, resourceId, 1, timeout, unit);
    }

    /**
     * Acquires permits from this {@link RateLimiter} if it can be acquired immediately without delay.
     *
     * <p>This method is equivalent to {@code tryAcquire(permits, 0, anyUnit)}.
     *
     * @param permits the number of permits to acquire
     * @return {@code true} if the permits were acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     * @since 14.0
     */
    default boolean tryConsume(Object context, K resourceId, int permits) {
        return tryConsume(context, resourceId, permits, 0, MICROSECONDS);
    }

    /**
     * Acquires a permit from this {@link RateLimiter} if it can be acquired immediately without
     * delay.
     *
     * <p>This method is equivalent to {@code tryAcquire(1)}.
     *
     * @return {@code true} if the permit was acquired, {@code false} otherwise
     * @since 14.0
     */
    default boolean tryConsume(Object context, K resourceId) {
        return tryConsume(context, resourceId, 1, 0, MICROSECONDS);
    }

    /**
     * Acquires the given number of permits from this {@code RateLimiter} if it can be obtained
     * without exceeding the specified {@code timeout}, or returns {@code false} immediately (without
     * waiting) if the permits would not have been granted before the timeout expired.
     *
     * @param permits the number of permits to acquire
     * @param timeout the maximum time to wait for the permits. Negative values are treated as zero.
     * @return {@code true} if the permits were acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     * @since 28.0
     */
    default boolean tryConsume(Object context, K resourceId, int permits, Duration timeout) {
        return tryConsume(context, resourceId, permits, Util.toNanosSaturated(timeout), TimeUnit.NANOSECONDS);
    }

    /**
     * Consumes the given number of permits from this {@code RateLimiter} if it can be obtained
     * without exceeding the specified {@code timeout}, or returns {@code false} immediately (without
     * waiting) if the permits would not have been granted before the timeout expired.
     *
     * @param permits the number of permits to acquire
     * @param timeout the maximum time to wait for the permits. Negative values are treated as zero.
     * @param unit the time unit of the timeout argument
     * @return {@code true} if the permits were acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    boolean tryConsume(Object context, K resourceId, int permits, long timeout, TimeUnit unit);

    /**
     * Returns a composed RateLimiter that first calls this RateLimiter's tryConsume function,
     * and then calls the tryConsume function of the {@code after} RateLimiter.
     * If evaluation of either tryConsume function throws an exception, it is relayed to
     * the caller of the composed function. Best effort is made to call both tryConsume functions
     * before relaying the thrown exception.
     *
     * @param after The RateLimiter to tryConsume after this RateLimiter is incremented
     * @return a composed RateLimiter that first increments this and then the {@code after} RateLimiter
     * @throws NullPointerException if after is null
     */
    default RateLimiter<K> andThen(RateLimiter<K> after) {
        Objects.requireNonNull(after);
        return (context, resourceId, permits, timeout, unit) -> {
            final boolean a = RateLimiter.this.tryConsume(context, resourceId, permits, timeout, unit);
            final boolean b = after.tryConsume(context, resourceId, permits, timeout, unit);
            return a && b;
        };
    }
}
