package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.cache.InMemoryRateCache;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.rates.Rates;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class DefaultRateLimiter<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultRateLimiter.class);

    private final RateCache<K> cache;

    private final RateSupplier rateSupplier;

    private final Rates.Logic logic;

    private final Rate [] limits;

    private final RateExceededHandler rateExceededHandler;

    public DefaultRateLimiter(Rate... limits) {
        this(new InMemoryRateCache<>(), new LimitWithinDurationSupplier(), Rates.Logic.OR, new RateExceededExceptionThrower(), limits);
    }

    public DefaultRateLimiter(
            RateCache<K> cache,
            RateSupplier rateSupplier,
            Rates.Logic logic,
            RateExceededHandler rateExceededHandler,
            Rate... limits) {
        this.cache = Objects.requireNonNull(cache);
        this.rateSupplier = Objects.requireNonNull(rateSupplier);
        this.logic = Objects.requireNonNull(logic);
        this.limits = new Rate[limits.length];
        System.arraycopy(limits, 0, this.limits, 0, limits.length);
        this.rateExceededHandler = Objects.requireNonNull(rateExceededHandler);
    }

    @Override
    public Rate record(K key) throws RateLimitExceededException {

        Rate firstExceededLimit = null;

        final Rate existingRate = cache.get(key);

        final Rate next = existingRate == null ? getInitialRate() : existingRate.increment();

        boolean reset = false;

        if(limits.length > 0) {
            int resetCount = 0;
            for(Rate limit : limits) {
                final int n = next.compareTo(limit);
//                LOG.trace("Result: {}, for {} compareTo {}", n, next, limit);
                if(n == 0) {
                    ++resetCount;
                }else if(n > 0) {
                    if(firstExceededLimit == null) {
                        firstExceededLimit = limit;
                    }
                    if(isOr()) {
                        break;
                    }
                }else {
                    if(isAnd()) {
                        firstExceededLimit = null;
                        break;
                    }
                }
            }
            if((isAnd() && resetCount == limits.length)
                    || (isOr() && resetCount > 0)) {
                reset = true;
            }
        }

        if(LOG.isDebugEnabled()) {
            LOG.debug("For: {}, limit exceeded: {}, rate: {}, limits: {}",
                    key, firstExceededLimit != null, next, Arrays.toString(limits));
        }

        if(reset) {
            cache.put(key, getInitialRate());
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

    private boolean isOr() {
        return logic == Rates.Logic.OR;
    }

    private boolean isAnd() {
        return logic == Rates.Logic.AND;
    }

    private Rate getInitialRate() {
        return Objects.requireNonNull(rateSupplier.getInitialRate());
    }

    @Override
    public String toString() {
        return "DefaultRateLimiter{" +
                "logic=" + logic +
                ", limits=" + Arrays.toString(limits) +
                '}';
    }
}
