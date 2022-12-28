package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.Rates;

public interface RateLimiterFactory<K> {

    static <K> RateLimiterFactory<K> of() {
        return new DefaultRateLimiterFactory<>();
    }

    default RateLimiter<K> createNew(Rates rates) {
        return createNew(RateLimiterConfig.of(), rates);
    }

    RateLimiter<K> createNew(RateLimiterConfig<K, ?> rateLimiterConfig, Rates rates);
}
