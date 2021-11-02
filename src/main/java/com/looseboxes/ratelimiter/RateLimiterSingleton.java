package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCacheSingleton;
import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Collection;
import java.util.Collections;

public class RateLimiterSingleton<K> extends RateLimiterImpl<K> {

    public RateLimiterSingleton(Rate first, Rate limit) {
        super(first, limit);
    }

    public RateLimiterSingleton(RateSupplier rateSupplier, Collection<Rate> limits) {
        this(null, rateSupplier, limits, new RateExceededExceptionThrower<>());
    }

    public RateLimiterSingleton(K key, RateSupplier rateSupplier, Collection<Rate> limits, RateExceededHandler<K> rateExceededHandler) {
        super(new RateCacheSingleton<>(key), rateSupplier, limits, rateExceededHandler);
    }
}
