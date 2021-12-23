package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.RateConfigList;

public class DefaultRateLimiterFactory<K> implements RateLimiterFactory<K> {

    @Override
    public RateLimiter<K> createRateLimiter(
            RateLimiterConfiguration<K, ?> rateLimiterConfiguration, RateConfigList rateConfigList) {
        return new SimpleRateLimiter<>(
                rateLimiterConfiguration.getRateCache(),
                rateLimiterConfiguration.getRateFactory(),
                rateLimiterConfiguration.getRateExceededListener(), rateConfigList);
    }
}
