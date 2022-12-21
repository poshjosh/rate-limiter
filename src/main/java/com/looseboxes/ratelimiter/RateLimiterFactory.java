package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;

public interface RateLimiterFactory<K> {

    static <K> RateLimiterFactory<K> newInstance() {
        return new DefaultRateLimiterFactory<>();
    }

    default RateLimiter<K> createRateLimiter(Bandwidths limit) {
        return createRateLimiter(RateLimiterConfig.newInstance(), limit);
    }

    RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Bandwidths limit);
}
