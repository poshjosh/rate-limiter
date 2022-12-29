package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MICROSECONDS;

public interface ResourceLimiter<K> {

    ResourceLimiter<Object> NO_OP = (context, resourceId, permits, timeout, unit) -> true;

    @SuppressWarnings("unchecked")
    static <T> ResourceLimiter<T> noop() {
        return (ResourceLimiter<T>)NO_OP;
    }

    static <K> ResourceLimiter<K> of(Bandwidth... limits) {
        return of(Bandwidths.of(limits));
    }

    static <K> ResourceLimiter<K> of(Bandwidths limits) {
        return of(ResourceLimiterConfig.of(), limits);
    }

    static <K> ResourceLimiter<K> of(ResourceLimiterConfig<K, ?> resourceLimiterConfig, Bandwidth limit) {
        return of(resourceLimiterConfig, Bandwidths.of(limit));
    }

    static <K> ResourceLimiter<K> of(ResourceLimiterConfig<K, ?> resourceLimiterConfig, Bandwidths limits) {
        return new DefaultResourceLimiter<>(resourceLimiterConfig, limits);
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
     * Acquires a permit from this {@code ResourceLimiter} if it can be obtained without exceeding the
     * specified {@code timeout}, or returns {@code false} immediately (without waiting) if the permit
     * would not have been granted before the timeout expired.
     *
     * <p>This method is equivalent to {@code tryAcquire(1, timeout)}.
     *
     * @param timeout the maximum time to wait for the permit. Negative values are treated as zero.
     * @return {@code true} if the permit was acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    default boolean tryConsume(Object context, K resourceId, Duration timeout) {
        return tryConsume(context, resourceId, 1, Util.toNanosSaturated(timeout), TimeUnit.NANOSECONDS);
    }

    /**
     * Acquires a permit from this {@code ResourceLimiter} if it can be obtained without exceeding the
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
     * Acquires permits from this {@link ResourceLimiter} if it can be acquired immediately without delay.
     *
     * <p>This method is equivalent to {@code tryAcquire(permits, 0, anyUnit)}.
     *
     * @param permits the number of permits to acquire
     * @return {@code true} if the permits were acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    default boolean tryConsume(Object context, K resourceId, int permits) {
        return tryConsume(context, resourceId, permits, 0, MICROSECONDS);
    }

    /**
     * Acquires a permit from this {@link ResourceLimiter} if it can be acquired immediately without
     * delay.
     *
     * <p>This method is equivalent to {@code tryAcquire(1)}.
     *
     * @return {@code true} if the permit was acquired, {@code false} otherwise
     */
    default boolean tryConsume(Object context, K resourceId) {
        return tryConsume(context, resourceId, 1, 0, MICROSECONDS);
    }

    /**
     * Acquires the given number of permits from this {@code ResourceLimiter} if it can be obtained
     * without exceeding the specified {@code timeout}, or returns {@code false} immediately (without
     * waiting) if the permits would not have been granted before the timeout expired.
     *
     * @param permits the number of permits to acquire
     * @param timeout the maximum time to wait for the permits. Negative values are treated as zero.
     * @return {@code true} if the permits were acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    default boolean tryConsume(Object context, K resourceId, int permits, Duration timeout) {
        return tryConsume(context, resourceId, permits, Util.toNanosSaturated(timeout), TimeUnit.NANOSECONDS);
    }

    /**
     * Consumes the given number of permits from this {@code ResourceLimiter} if it can be obtained
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
     * Returns a composed ResourceLimiter that first calls this ResourceLimiter's tryConsume function,
     * OR (||) then calls the tryConsume function of the {@code after} ResourceLimiter.
     * If evaluation of either tryConsume function throws an exception, it is relayed to
     * the caller of the composed function.
     *
     * @param after The ResourceLimiter to tryConsume after this ResourceLimiter
     * @return a composed ResourceLimiter that first calls this OR (||) then the {@code after} ResourceLimiter
     * @throws NullPointerException if after is null
     */
    default ResourceLimiter<K> orThen(ResourceLimiter<K> after) {
        Objects.requireNonNull(after);
        return new ResourceLimiter<K>() {
            @Override
            public boolean tryConsume(Object context, K resourceId, int permits, long timeout, TimeUnit unit) {
                return ResourceLimiter.this.tryConsume(context, resourceId, permits, timeout, unit)
                        || after.tryConsume(context, resourceId, permits, timeout, unit);
            }
        };
    }

    /**
     * Returns a composed ResourceLimiter that first calls this ResourceLimiter's tryConsume function,
     * AND (&&) then calls the tryConsume function of the {@code after} ResourceLimiter.
     * If evaluation of either tryConsume function throws an exception, it is relayed to
     * the caller of the composed function.
     *
     * @param after The ResourceLimiter to tryConsume after this ResourceLimiter
     * @return a composed ResourceLimiter that firsts calls this AND (&&) then the {@code after} ResourceLimiter
     * @throws NullPointerException if after is null
     */
    default ResourceLimiter<K> andThen(ResourceLimiter<K> after) {
        Objects.requireNonNull(after);
        return new ResourceLimiter<K>() {
            @Override
            public boolean tryConsume(Object context, K resourceId, int permits, long timeout, TimeUnit unit) {
                return ResourceLimiter.this.tryConsume(context, resourceId, permits, timeout, unit)
                        && after.tryConsume(context, resourceId, permits, timeout, unit);
            }
        };
    }
}
