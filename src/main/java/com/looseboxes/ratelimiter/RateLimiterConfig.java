package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;

public interface RateLimiterConfig<K, V> {

    static <K, V> RateLimiterConfigBuilder<K, V> builder() {
        return new DefaultRateLimiterConfig<>();
    }

    static <K, V> RateLimiterConfigBuilder<K, V> builder(RateLimiterConfig<K, V> rateLimiterConfig) {
        return new DefaultRateLimiterConfig<>(rateLimiterConfig);
    }

    static <K, V> RateLimiterConfig<K, V> newInstance() {
        return new DefaultRateLimiterConfig<>();
    }

    static <K, V> RateLimiterConfig<K, V> of(RateLimiterConfig<K, V> rateLimiterConfig) {
        return new DefaultRateLimiterConfig<>(rateLimiterConfig);
    }

    RateCache<K, V> getRateCache();

    RateFactory getRateFactory();

    RateRecordedListener getRateRecordedListener();
}
