package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.SingletonRateCache;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

import java.util.List;

public class RateLimiterWithSingletonCacheTest extends RateLimiterTest {

    @Override
    public RateLimiter<Object> getRateLimiter(List<RateConfig> limits) {
        return new SimpleRateLimiter<>(new RateLimiterConfiguration<>()
                .rateCache(new SingletonRateCache<>(null))
                .rateFactory(new LimitWithinDurationFactory())
                .rateRecordedListener(new RateExceededExceptionThrower()), new RateLimitConfig().addLimits(limits));
    }
}
