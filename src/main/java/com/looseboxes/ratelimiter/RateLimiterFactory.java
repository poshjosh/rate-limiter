package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.RateConfigList;

public interface RateLimiterFactory<K> {

    default RateLimiter<K> createRateLimiter(RateConfigList rateConfigList) {
        return createRateLimiter(new DefaultRateLimiterConfig<>(), rateConfigList);
    }

    RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, RateConfigList rateConfigList);
}
