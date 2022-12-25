package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.Rates;

final class DefaultRateLimiterFactory<K> implements RateLimiterFactory<K> {
    DefaultRateLimiterFactory() {}
    @Override
    public RateLimiter<K> createNew(RateLimiterConfig<K, ?> rateLimiterConfig, Rates limits) {
        return RateLimiter.of(rateLimiterConfig, limits);
    }
}
