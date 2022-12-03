package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Limit;

public class DefaultRateLimiterFactory<K> implements RateLimiterFactory<K> {

    @Override
    public RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Limit limit) {
        return RateLimiter.of(rateLimiterConfig, limit);
    }
}
