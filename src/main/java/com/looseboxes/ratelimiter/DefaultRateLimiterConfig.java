package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.MapRateCache;

public final class DefaultRateLimiterConfig<K, V> extends RateLimiterConfig<K, V>{
    public DefaultRateLimiterConfig() {
        super(new MapRateCache<>(), new LimitWithinDurationFactory(), new RateExceededExceptionThrower());
    }
}
