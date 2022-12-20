package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.CompositeRate;

final class DefaultRateLimiterFactory<K> implements RateLimiterFactory<K> {
    @Override
    public RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, CompositeRate limit) {
        return RateLimiter.of(rateLimiterConfig, limit);
    }
}
