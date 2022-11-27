package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.SingletonRateCache;
import com.looseboxes.ratelimiter.rates.Rate;

public class RateLimiterWithSingletonCacheTest extends RateLimiterTest {

    @Override
    public RateLimiter<String> getRateLimiter(Rate... rates) {
        return new SimpleRateLimiter<String>(rates).withRateCache(new SingletonRateCache<>(null));
    }
}
