package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.RateConfigList;

public interface RateLimiterFactory<K> {

    default RateLimiter<K> createRateLimiter(RateConfigList rateConfigList) {
        return createRateLimiter(new RateLimiterConfiguration<>(), rateConfigList);
    }

    RateLimiter<K> createRateLimiter(RateLimiterConfiguration<K, ?> rateLimiterConfiguration, RateConfigList rateConfigList);
}
