package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Limit;

public interface RateLimiterFactory<K> {

    default RateLimiter<K> createRateLimiter(Limit limit) {
        return createRateLimiter(RateLimiterConfig.newInstance(), limit);
    }

    RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Limit limit);
}
