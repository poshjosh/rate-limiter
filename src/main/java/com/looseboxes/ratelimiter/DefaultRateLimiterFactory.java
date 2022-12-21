package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;

final class DefaultRateLimiterFactory<K> implements RateLimiterFactory<K> {
    @Override
    public RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Bandwidths limit) {
        return RateLimiter.of(rateLimiterConfig, limit);
    }
}
