package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.Rates;

public interface RateLimiterFactory<K> {

    static <K> RateLimiterFactory<K> newInstance() {
        return new DefaultRateLimiterFactory<>();
    }

    RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Rates rates);
}
