package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;

class RateLimiterWithSingletonCacheTest extends AbstractRateLimiterTest {

    public RateLimiterWithSingletonCacheTest() {
        super(BandwidthFactory.bursty(),true);
    }

    @Override
    public RateLimiter<String> getRateLimiter(Bandwidth... rates) {
        RateLimiterConfig<String, ?> config =
                RateLimiterConfig.<String, Object>builder().rateCache(RateCache.singleton()).build();
        return RateLimiter.<String>of(config, Bandwidths.of(rates));
    }
}
