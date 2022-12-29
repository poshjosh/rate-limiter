package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;

public interface ResourceLimiterConfig<K, V> {

    interface Builder<K, V> {

        ResourceLimiterConfig<K, V> build();

        Builder<K, V> cache(RateCache<K, V> rateCache);

        Builder<K, V> usageListener(ResourceUsageListener resourceUsageListener);

        Builder<K, V> rateLimiterProvider(RateLimiterProvider<K> rateLimiterProvider);
    }

    static <K, V> Builder<K, V> builder() {
        return new DefaultResourceLimiterConfig<>();
    }

    static <K, V> Builder<K, V> builder(ResourceLimiterConfig<K, V> resourceLimiterConfig) {
        return new DefaultResourceLimiterConfig<>(resourceLimiterConfig);
    }

    static <K, V> ResourceLimiterConfig<K, V> of() {
        return new DefaultResourceLimiterConfig<>();
    }

    static <K, V> ResourceLimiterConfig<K, V> of(ResourceLimiterConfig<K, V> resourceLimiterConfig) {
        return new DefaultResourceLimiterConfig<>(resourceLimiterConfig);
    }

    RateCache<K, V> getCache();

    ResourceUsageListener getUsageListener();

    RateLimiterProvider<K> getRateLimiterProvider();
}
