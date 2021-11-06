package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCacheSingleton;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.rates.Rates;

import java.util.Collection;
import java.util.Collections;

/**
 * A rate limiter that accepts a single key, for recording/limiting a single candidate
 *
 * Will throw an exception, if a rate is recorded with a key that does not match the specified key.
 * If the specified key is null, then rates may be recorded with any key.
 *
 * @param <K>
 */
public class RateLimiterSingleton<K> extends RateLimiterImpl<K> {

    public RateLimiterSingleton(Rate first, Rate limit) {
        super(first, limit);
    }

    public RateLimiterSingleton(RateSupplier rateSupplier, Collection<Rate> limits) {
        this(null, rateSupplier, Rates.Logic.OR, limits, new RateExceededExceptionThrower<>());
    }

    public RateLimiterSingleton(K key, RateSupplier rateSupplier, Rates.Logic logic, Collection<Rate> limits, RateExceededHandler<K> rateExceededHandler) {
        super(new RateCacheSingleton<>(key), rateSupplier, logic, limits, rateExceededHandler);
    }
}
