package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.Rates;

public interface RateLimiterFactory<K> {

    static <K> RateLimiterFactory<K> of() {
        return new DefaultRateLimiterFactory<>();
    }

    RateLimiter<K> createNew(RateLimiterConfig<K, ?> rateLimiterConfig, Rates rates);
}
