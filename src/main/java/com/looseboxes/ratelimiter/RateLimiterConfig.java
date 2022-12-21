package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;

public interface RateLimiterConfig<K, V> {

    interface Builder<K, V> {

        RateLimiterConfig<K, V> build();

        Builder<K, V> rateCache(RateCache<K, V> rateCache);

        Builder<K, V> rateRecordedListener(RateRecordedListener rateRecordedListener);

        Builder<K, V> bandwidthLimiterProvider(BandwidthLimiterProvider<K> bandwidthLimiterProvider);
    }

    static <K, V> Builder<K, V> builder() {
        return new DefaultRateLimiterConfig<>();
    }

    static <K, V> Builder<K, V> builder(RateLimiterConfig<K, V> rateLimiterConfig) {
        return new DefaultRateLimiterConfig<>(rateLimiterConfig);
    }

    static <K, V> RateLimiterConfig<K, V> newInstance() {
        return new DefaultRateLimiterConfig<>();
    }

    static <K, V> RateLimiterConfig<K, V> of(RateLimiterConfig<K, V> rateLimiterConfig) {
        return new DefaultRateLimiterConfig<>(rateLimiterConfig);
    }

    RateCache<K, V> getRateCache();

    RateRecordedListener getRateRecordedListener();

    BandwidthLimiterProvider<K> getBandwidthLimiterProvider();
}
