package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.RateConfigList;

public class DefaultRateLimiterFactory<K> implements RateLimiterFactory<K> {

    @Override
    public RateLimiter<K> createRateLimiter(
            RateLimiterConfig<K, ?> rateLimiterConfig, RateConfigList rateConfigList) {
        return new SimpleRateLimiter<>(
                rateLimiterConfig.getRateCache(),
                rateLimiterConfig.getRateFactory(),
                rateLimiterConfig.getRateRecordedListener(), rateConfigList);
    }
}
