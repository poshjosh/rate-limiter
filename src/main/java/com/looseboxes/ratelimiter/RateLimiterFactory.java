package com.looseboxes.ratelimiter;

public interface RateLimiterFactory<K> {

    default RateLimiter<K> createRateLimiter(Limit limit) {
        return createRateLimiter(new DefaultRateLimiterConfig<>(), limit);
    }

    RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Limit limit);
}
