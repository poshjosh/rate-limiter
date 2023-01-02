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
    private KeyProvider<R, K> keyProvider;
    private ResourceUsageListener usageListener;
    private RateLimiterProvider<K> rateLimiterProvider;

    ResourceLimiterConfig() {
        this(RateCache.noop(),
                KeyProvider.identity(),
                ResourceUsageListener.NO_OP,
                new RateLimiterProvider<>(SleepingTicker.zeroOffset()));
    }

    ResourceLimiterConfig(ResourceLimiterConfig<R, K> resourceLimiterConfig) {
        this(resourceLimiterConfig.getCache(),
                resourceLimiterConfig.getKeyProvider(),
                resourceLimiterConfig.getUsageListener(),
                resourceLimiterConfig.getRateLimiterProvider());
    }

    ResourceLimiterConfig(
            RateCache<K, Bandwidths> cache,
            KeyProvider<R, K> keyProvider,
            ResourceUsageListener usageListener,
            RateLimiterProvider<K> rateLimiterProvider) {
        this.cache = Objects.requireNonNull(cache);
        this.keyProvider = Objects.requireNonNull(keyProvider);
        this.usageListener = Objects.requireNonNull(usageListener);
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

    public ResourceLimiterConfig<R, K> keyProvider(KeyProvider<R, K> keyProvider) {
        this.setKeyProvider(keyProvider);
        return this;
    }

    public KeyProvider<R, K> getKeyProvider() {
        return keyProvider;
    }

    public void setKeyProvider(KeyProvider<R, K> keyProvider) {
        this.keyProvider = keyProvider;
    }

    public ResourceLimiterConfig<R, K> usageListener(ResourceUsageListener resourceUsageListener) {
        this.setUsageListener(resourceUsageListener);
        return this;
    }

    public ResourceUsageListener getUsageListener() {
        return usageListener;
    }

    public void setUsageListener(ResourceUsageListener resourceUsageListener) {
        this.usageListener = resourceUsageListener;
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
