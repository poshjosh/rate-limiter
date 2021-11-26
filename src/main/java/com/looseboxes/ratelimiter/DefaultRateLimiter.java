package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.InMemoryRateCache;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class DefaultRateLimiter<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultRateLimiter.class);

    private final RateCache<K> cache;

    private final RateFactory rateFactory;

    private final Logic logic;

    private final Rate [] limits;

    private final RateRecordedListener rateRecordedListener;

    public DefaultRateLimiter(RateConfig rateConfig) {
        this(new RateLimitConfig().addLimit(rateConfig));
    }

    public DefaultRateLimiter(RateLimitConfig rateLimitConfig) {
        this(new RateLimiterConfiguration<K>()
                .rateCache(new InMemoryRateCache<>())
                .rateFactory(new LimitWithinDurationFactory())
                .rateRecordedListener(new RateExceededExceptionThrower())
                .rateLimitConfig(rateLimitConfig));
    }

    public DefaultRateLimiter(RateLimiterConfiguration<K> rateLimiterConfiguration) {
        this.cache = Objects.requireNonNull(rateLimiterConfiguration.getRateCache());
        this.rateFactory = Objects.requireNonNull(rateLimiterConfiguration.getRateFactory());
        this.logic = Objects.requireNonNull(rateLimiterConfiguration.getRateLimitConfig().getLogic());
        this.limits = rateLimiterConfiguration.getRateLimitConfig().toRateList().toArray(new Rate[0]);
        this.rateRecordedListener = Objects.requireNonNull(rateLimiterConfiguration.getRateRecordedListener());
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

        final Rate result = reset ? getInitialRate() : next;
        if(existingRate != result) {
            cache.put(key, result);
        }

        rateRecordedListener.onRateRecorded(key, result);

        if(firstExceededLimit != null) {
            rateRecordedListener.onRateExceeded(key, result, firstExceededLimit);
        }

        return result;
    }

    private boolean isOr() {
        return logic == Logic.OR;
    }

    private boolean isAnd() {
        return logic == Logic.AND;
    }

    private Rate getInitialRate() {
        return Objects.requireNonNull(rateFactory.createNew());
    }

    @Override
    public String toString() {
        return "DefaultRateLimiter{" +
                "logic=" + logic +
                ", limits=" + Arrays.toString(limits) +
                '}';
    }
}
