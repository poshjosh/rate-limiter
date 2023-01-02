package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.cache.RateCache;

import java.time.Duration;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MICROSECONDS;

public interface ResourceLimiter<R> {

    ResourceLimiter<Object> NO_OP = new ResourceLimiter<Object>() {
        @Override public boolean tryConsume(Object res, int permits, long timeout, TimeUnit unit) {
            return true;
        }
        @Override public <K> ResourceLimiter<Object> keyProvider(KeyProvider<Object, K> keyProv) {
            return this;
        }
        @Override public <K> ResourceLimiter<Object> cache(RateCache<K, Bandwidths> cache) {
            return this;
        }
        @Override public ResourceLimiter<Object> listener(ResourceUsageListener listener) {
            return this;
        }
    };

    @SuppressWarnings("unchecked")
    static <R> ResourceLimiter<R> noop() {
        return (ResourceLimiter<R>)NO_OP;
    }

    static <R> ResourceLimiter<R> of(Bandwidth... limits) {
        return of(Bandwidths.of(limits));
    }

    static <R> ResourceLimiter<R> of(Bandwidth limit) {
        return of(Bandwidths.of(limit));
    }

    static <R> ResourceLimiter<R> of(Bandwidths limits) {
        return new DefaultResourceLimiter<>(ResourceLimiterConfig.ofDefaults(), limits);
    }

    /**
     * Consumes the given number of permits from this {@code ResourceLimiter} if it can be obtained
     * without exceeding the specified {@code timeout}, or returns {@code false} immediately (without
     * waiting) if the permits would not have been granted before the timeout expired.
     *
     * @param resource the resource to acquire permits for
     * @param permits the number of permits to acquire
     * @param timeout the maximum time to wait for the permits. Negative values are treated as zero.
     * @param unit the time unit of the timeout argument
     * @return {@code true} if the permits were acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    boolean tryConsume(R resource, int permits, long timeout, TimeUnit unit);

    <K> ResourceLimiter<R> keyProvider(KeyProvider<R, K> keyProvider);
    
    <K> ResourceLimiter<R> cache(RateCache<K, Bandwidths> cache);
    
    ResourceLimiter<R> listener(ResourceUsageListener resourceUsageListener);

    /**
     * Acquires a permit from this {@link ResourceLimiter} if it can be acquired immediately without
     * delay.
     *
     * <p>This method is equivalent to {@code tryAcquire(1)}.
     *
     * @param resource the resource to acquire permits for
     * @return {@code true} if the permit was acquired, {@code false} otherwise
     */
    default boolean tryConsume(R resource) {
        return tryConsume(resource, 1, 0, MICROSECONDS);
    }

    /**
     * Acquires a permit from this {@code ResourceLimiter} if it can be obtained without exceeding the
     * specified {@code timeout}, or returns {@code false} immediately (without waiting) if the permit
     * would not have been granted before the timeout expired.
     *
     * <p>This method is equivalent to {@code tryAcquire(1, timeout)}.
     *
     * @param resource the resource to acquire permits for
     * @param timeout the maximum time to wait for the permit. Negative values are treated as zero.
     * @return {@code true} if the permit was acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    default boolean tryConsume(R resource, Duration timeout) {
        return tryConsume(resource, 1, Util.toNanosSaturated(timeout), TimeUnit.NANOSECONDS);
    }

    /**
     * Acquires a permit from this {@code ResourceLimiter} if it can be obtained without exceeding the
     * specified {@code timeout}, or returns {@code false} immediately (without waiting) if the permit
     * would not have been granted before the timeout expired.
     *
     * <p>This method is equivalent to {@code tryAcquire(1, timeout, unit)}.
     *
     * @param resource the resource to acquire permits for
     * @param timeout the maximum time to wait for the permit. Negative values are treated as zero.
     * @param unit the time unit of the timeout argument
     * @return {@code true} if the permit was acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    default boolean tryConsume(R resource, long timeout, TimeUnit unit) {
        return tryConsume(resource, 1, timeout, unit);
    }

    /**
     * Acquires permits from this {@link ResourceLimiter} if it can be acquired immediately without delay.
     *
     * <p>This method is equivalent to {@code tryAcquire(permits, 0, anyUnit)}.
     *
     * @param resource the resource to acquire permits for
     * @param permits the number of permits to acquire
     * @return {@code true} if the permits were acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    default boolean tryConsume(R resource, int permits) {
        return tryConsume(resource, permits, 0, MICROSECONDS);
    }

    /**
     * Acquires the given number of permits from this {@code ResourceLimiter} if it can be obtained
     * without exceeding the specified {@code timeout}, or returns {@code false} immediately (without
     * waiting) if the permits would not have been granted before the timeout expired.
     *
     * @param resource the resource to acquire permits for
     * @param permits the number of permits to acquire
     * @param timeout the maximum time to wait for the permits. Negative values are treated as zero.
     * @return {@code true} if the permits were acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    default boolean tryConsume(R resource, int permits, Duration timeout) {
        return tryConsume(resource, permits, Util.toNanosSaturated(timeout), TimeUnit.NANOSECONDS);
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
    default ResourceLimiter<R> andThen(ResourceLimiter<R> after) {
        Objects.requireNonNull(after);
        return new ResourceLimiter<R>() {
            @Override
            public boolean tryConsume(R resource, int permits, long timeout, TimeUnit unit) {
                return ResourceLimiter.this.tryConsume(resource, permits, timeout, unit)
                        && after.tryConsume(resource, permits, timeout, unit);
            }
            @Override public <K> ResourceLimiter<R> keyProvider(KeyProvider<R, K> keyProvider) {
                return ResourceLimiter.this.keyProvider(keyProvider).andThen(after.keyProvider(
                        keyProvider));
            }
            @Override public <K> ResourceLimiter<R> cache(RateCache<K, Bandwidths> cache) {
                return ResourceLimiter.this.cache(cache).andThen(after.cache(cache));
            }
            @Override public ResourceLimiter<R> listener(ResourceUsageListener listener) {
                return ResourceLimiter.this.listener(listener).andThen(after.listener(listener));
            }
        };
    }
}
