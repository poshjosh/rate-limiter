package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.Rates;

final class DefaultRateLimiterFactory<K> implements RateLimiterFactory<K> {
    @Override
    public RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Rates limits) {
        return RateLimiter.of(rateLimiterConfig, limits);
    }
}
