package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.SingletonRateCache;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.util.List;

public class RateLimiterWithSingletonCacheTest extends RateLimiterTest {

    @Override
    public RateLimiter<Object> getRateLimiter(List<RateConfig> limits) {
        return new DefaultRateLimiter<>(new RateLimiterConfiguration<>()
                .rateCache(new SingletonRateCache<>(null))
                .rateSupplier(new LimitWithinDurationSupplier())
                .rateExceededHandler(new RateExceededExceptionThrower())
                .rateLimitConfig(new RateLimitConfig().addLimits(limits)));
    }
}