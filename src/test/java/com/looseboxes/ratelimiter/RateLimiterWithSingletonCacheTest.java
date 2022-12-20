package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.CompositeRate;

class RateLimiterWithSingletonCacheTest extends AbstractRateLimiterTest {

    public RateLimiterWithSingletonCacheTest() {
        super(true);
    }

    @Override
    public RateLimiter<String> getRateLimiter(Rate... rates) {
        RateLimiterConfig<String, ?> config =
                RateLimiterConfig.<String, Object>builder().rateCache(RateCache.singleton()).build();
        return RateLimiter.<String>of(config, CompositeRate.of(rates));
    }
}
