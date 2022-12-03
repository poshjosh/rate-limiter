package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;

public interface RateLimiterConfigBuilder<K, V> {

    RateLimiterConfig<K, V> build();

    RateLimiterConfigBuilder<K, V> rateCache(RateCache<K, V> rateCache);

    RateLimiterConfigBuilder<K, V> rateFactory(RateFactory rateFactory);

    RateLimiterConfigBuilder<K, V> rateRecordedListener(RateRecordedListener rateRecordedListener);
}
