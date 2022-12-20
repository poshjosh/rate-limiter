package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.CompositeRate;

public interface RateLimiterFactory<K> {

    static <K> RateLimiterFactory<K> newInstance() {
        return new DefaultRateLimiterFactory<>();
    }

    default RateLimiter<K> createRateLimiter(CompositeRate limit) {
        return createRateLimiter(RateLimiterConfig.newInstance(), limit);
    }

    RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, CompositeRate limit);
}
