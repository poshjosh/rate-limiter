package com.looseboxes.ratelimiter.experimental;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.rates.Rates;
import com.looseboxes.ratelimiter.util.Experimental;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Objects;

@Experimental
public class RateLimiterCompositeRate<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultRateLimiter.class);

    private final RateCache<K> cache;

    private final RateSupplier rateSupplier;

    private final Rate limit;

    private final RateRecordedListener rateRecordedListener;

    public RateLimiterCompositeRate(
            RateCache<K> cache,
            RateSupplier rateSupplier,
            Collection<Rate> limit,
            RateRecordedListener rateRecordedListener) {
        this(cache, rateSupplier, Rates.or(limit.toArray(new Rate[0])), rateRecordedListener);
    }

    public RateLimiterCompositeRate(
            RateCache<K> cache,
            RateSupplier rateSupplier,
            Rate limit,
            RateRecordedListener rateRecordedListener) {
        this.cache = Objects.requireNonNull(cache);
        this.rateSupplier = Objects.requireNonNull(rateSupplier);
        this.limit = Objects.requireNonNull(limit);
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
    }

    @Override
    public Rate record(K key) throws RateLimitExceededException {

        final Rate existingRate = cache.get(key);

        final Rate next = existingRate == null ? getInitialRate() : existingRate.increment();

        final int n = next.compareTo(limit);

        final boolean reset = n == 0;

        if(LOG.isDebugEnabled()) {
            LOG.debug("For: {}, rate: {} exceeds: {}, limit: {}", key, next, n > 0, limit);
        }

        if(reset) {
            cache.remove(key);
        }else{
            if(existingRate != next) {
                cache.put(key, next);
            }
        }

        if(n > 0) {
            rateRecordedListener.onRateExceeded(key, next, limit);
        }

        return reset ? Rate.NONE : next;
    }

    private Rate getInitialRate() {
        return Objects.requireNonNull(rateSupplier.getInitialRate());
    }

    @Override
    public String toString() {
        return "DefaultRateLimiter{" +
                "limit=" + limit +
                '}';
    }
}
