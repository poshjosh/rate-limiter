package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;

public interface RateLimiterConfig<K, V> {

    RateCache<K, V> getRateCache();

    RateFactory getRateFactory();

    RateRecordedListener getRateRecordedListener();
}
