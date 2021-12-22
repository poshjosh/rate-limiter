package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.SingletonRateCache;
import com.looseboxes.ratelimiter.util.RateConfig;

import java.util.List;

public class RateLimiterWithSingletonCacheTest extends RateLimiterTest {

    @Override
    public RateLimiter<String> getRateLimiter(List<RateConfig> limits) {
        return new SimpleRateLimiter<String>(limits).withRateCache(new SingletonRateCache<>(null));
    }
}
