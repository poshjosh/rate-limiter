package com.looseboxes.ratelimiter;

public class DefaultRateLimiterFactory<K> implements RateLimiterFactory<K> {

    @Override
    public RateLimiter<K> createRateLimiter(
            RateLimiterConfig<K, ?> rateLimiterConfig, Limit limit) {
        return new SimpleRateLimiter<>(
                rateLimiterConfig.getRateCache(),
                rateLimiterConfig.getRateFactory(),
                rateLimiterConfig.getRateRecordedListener(),
                limit);
    }
}
