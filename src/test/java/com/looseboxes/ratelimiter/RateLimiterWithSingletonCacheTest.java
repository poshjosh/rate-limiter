package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.rates.Limit;
import com.looseboxes.ratelimiter.rates.Rate;

public class RateLimiterWithSingletonCacheTest extends RateLimiterTest {

    @Override
    public RateLimiter<String> getRateLimiter(Rate... rates) {
    RateLimiterConfig<String, ?> config =
        RateLimiterConfig.<String, Object>builder().rateCache(RateCache.singleton()).build();
        return RateLimiter.<String>of(config, Limit.of(rates));
    }
}
