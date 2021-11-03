package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.cache.RateCacheInMemory;
import com.looseboxes.ratelimiter.rates.Rate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class RateLimiterImpl<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(RateLimiterImpl.class);

    private final RateCache<K> cache;

    private final RateSupplier rateSupplier;

    private final List<Rate> limits;

    private final RateExceededHandler<K> rateExceededHandler;

    public RateLimiterImpl(Rate first, Rate limit) {
        this(() -> first, Collections.singletonList(limit));
    }

    public RateLimiterImpl(RateSupplier rateSupplier, Collection<Rate> limits) {
        this(new RateCacheInMemory<>(), rateSupplier, limits, new RateExceededExceptionThrower<>());
    }

    public RateLimiterImpl(
            RateCache<K> cache,
            RateSupplier rateSupplier,
            Collection<Rate> limits,
            RateExceededHandler<K> rateExceededHandler) {
        this.cache = Objects.requireNonNull(cache);
        this.rateSupplier = Objects.requireNonNull(rateSupplier);
        this.limits = Collections.unmodifiableList(new ArrayList<>(limits));
        this.rateExceededHandler = Objects.requireNonNull(rateExceededHandler);
    }

    @Override
    public Rate record(K key) throws RateLimitExceededException {

        Rate firstExceededLimit = null;

        final Rate existingRate = cache.get(key);

        final Rate next = existingRate == null ? getInitialRate() : existingRate.increment();

        boolean reset = false;

        if(!limits.isEmpty()) {
            int resetCount = 0;
            for(Rate limit : limits) {
                final int n = next.compareTo(limit);
                LOG.trace("Result: {}, for {} compareTo {}", n, next, limit);
                if(n == 0) {
                    ++resetCount;
                }else if(n > 0) {
                    if(firstExceededLimit == null) {
                        firstExceededLimit = limit;
                        break;
                    }
                }
            }
            if(resetCount == limits.size()) {
                reset = true;
            }
        }

        if(LOG.isDebugEnabled()) {
            LOG.debug("For: {}, rate: {} exceeds: {}, limit: {}",
                    key, next, firstExceededLimit != null, firstExceededLimit);
        }

        if(reset) {
            cache.remove(key);
        }else{
            if(existingRate != next) {
                cache.put(key, next);
            }
        }

        if(firstExceededLimit != null) {
            rateExceededHandler.onRateExceeded(key, next, firstExceededLimit);
        }

        return reset ? Rate.NONE : next;
    }

    private Rate getInitialRate() {
        return Objects.requireNonNull(rateSupplier.getInitialRate());
    }

    @Override
    public String toString() {
        return "RateLimiterImpl{" +
                "limits=" + limits +
                '}';
    }
}
