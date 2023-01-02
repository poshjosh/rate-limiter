package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.SleepingTicker;

import java.util.Objects;

/**
 * @param <R> The type of the resource
 * @param <K> The type of the cache key for the resource
 */
final class ResourceLimiterConfig<R, K> {

    static <R, K> ResourceLimiterConfig<R, K> ofDefaults() {
        return new ResourceLimiterConfig<>();
    }

    static <R, K> ResourceLimiterConfig<R, K> of(ResourceLimiterConfig<R, K> resourceLimiterConfig) {
        return new ResourceLimiterConfig<>(resourceLimiterConfig);
    }

    private RateCache<K, Bandwidths> cache;
    private UsageListener listener;
    private RateLimiterProvider<K> rateLimiterProvider;

    ResourceLimiterConfig() {
        this(RateCache.noop(),
                UsageListener.NO_OP,
                new RateLimiterProvider<>(SleepingTicker.zeroOffset()));
    }

    ResourceLimiterConfig(ResourceLimiterConfig<R, K> resourceLimiterConfig) {
        this(resourceLimiterConfig.getCache(),
                resourceLimiterConfig.getListener(),
                resourceLimiterConfig.getRateLimiterProvider());
    }

    ResourceLimiterConfig(
            RateCache<K, Bandwidths> cache,
            UsageListener listener,
            RateLimiterProvider<K> rateLimiterProvider) {
        this.cache = Objects.requireNonNull(cache);
        this.listener = Objects.requireNonNull(listener);
        this.rateLimiterProvider = Objects.requireNonNull(rateLimiterProvider);
    }

    public ResourceLimiterConfig<R, K> build() {
        return new ResourceLimiterConfig<>(this);
    }

    public ResourceLimiterConfig<R, K> cache(RateCache<K, Bandwidths> rateCache) {
        this.setCache(rateCache);
        return this;
    }

    public RateCache<K, Bandwidths> getCache() {
        return cache;
    }

    public void setCache(RateCache<K, Bandwidths> cache) {
        this.cache = cache;
    }

    public ResourceLimiterConfig<R, K> listener(UsageListener usageListener) {
        this.setListener(usageListener);
        return this;
    }

    public UsageListener getListener() {
        return listener;
    }

    public void setListener(UsageListener listener) {
        this.listener = listener;
    }

    public ResourceLimiterConfig<R, K> rateLimiterProvider(
            RateLimiterProvider<K> rateLimiterProvider) {
        this.rateLimiterProvider = rateLimiterProvider;
        return this;
    }

    public RateLimiterProvider<K> getRateLimiterProvider() {
        return rateLimiterProvider;
    }

    public void setRateLimiterProvider(RateLimiterProvider<K> rateLimiterProvider) {
        this.rateLimiterProvider = rateLimiterProvider;
    }
}
