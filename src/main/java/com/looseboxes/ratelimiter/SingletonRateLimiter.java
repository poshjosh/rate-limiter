package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.SingletonRateCache;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.rates.Rates;

/**
 * A rate limiter that accepts a single key, for recording/limiting a single candidate
 *
 * Will throw an exception, if a rate is recorded with a key that does not match the specified key.
 * If the specified key is null, then rates may be recorded with any key.
 *
 * @param <K>
 */
public class SingletonRateLimiter<K> extends DefaultRateLimiter<K> {

    public SingletonRateLimiter(Rate... limits) {
        this(null, new LimitWithinDurationSupplier(), Rates.Logic.OR, new RateExceededExceptionThrower(), limits);
    }

    public SingletonRateLimiter(K key, RateSupplier rateSupplier, Rates.Logic logic, RateExceededHandler rateExceededHandler, Rate... limits) {
        super(new SingletonRateCache<>(key), rateSupplier, logic, rateExceededHandler, limits);
    }
}
