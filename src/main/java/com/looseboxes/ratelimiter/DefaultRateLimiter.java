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

    private final RateLimiter<K> parent;

    private final RateCache<Object> cache;

    private final RateFactory rateFactory;

    private final Logic logic;

    private final Rate [] limits;

    private final RateExceededListener rateExceededListener;

    public DefaultRateLimiter(RateConfig rateConfig) {
        this(RateLimiter.noop(), rateConfig);
    }

    public DefaultRateLimiter(RateLimiter<K> parent, RateConfig rateConfig) {
        this(parent, new RateLimitConfig().addLimit(rateConfig));
    }

    public DefaultRateLimiter(RateLimitConfig rateLimitConfig) {
        this(RateLimiter.noop(), rateLimitConfig);
    }

    public DefaultRateLimiter(RateLimiter<K> parent, RateLimitConfig rateLimitConfig) {
        this(parent, new RateLimiterConfiguration<>()
                .rateCache(new InMemoryRateCache<>())
                .rateFactory(new LimitWithinDurationFactory())
                .rateRecordedListener(new RateExceededExceptionThrower())
                .rateLimitConfig(rateLimitConfig));
    }

    public DefaultRateLimiter(RateLimiterConfiguration<Object> rateLimiterConfiguration) {
        this(RateLimiter.noop(), rateLimiterConfiguration);
    }

    @SuppressWarnings("unchecked")
    public DefaultRateLimiter(RateLimiter<K> parent, RateLimiterConfiguration<?> rateLimiterConfiguration) {
        this.parent = Objects.requireNonNull(parent);
        this.cache = Objects.requireNonNull((RateCache<Object>)rateLimiterConfiguration.getRateCache());
        this.rateFactory = Objects.requireNonNull(rateLimiterConfiguration.getRateFactory());
        this.logic = Objects.requireNonNull(rateLimiterConfiguration.getRateLimitConfig().getLogic());
        this.limits = rateLimiterConfiguration.getRateLimitConfig().toRateList().toArray(new Rate[0]);
        this.rateExceededListener = Objects.requireNonNull(rateLimiterConfiguration.getRateRecordedListener());
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

        try {
            if(firstExceededLimit != null) {
                rateExceededListener
                        .onRateExceeded(new RateExceededEvent(this, key, result, firstExceededLimit));
            }
        }finally{
            parent.record(key);
        }

        return result;
    }

    protected boolean isOr() {
        return logic == Logic.OR;
    }

    protected boolean isAnd() {
        return logic == Logic.AND;
    }

    protected Rate getInitialRate() {
        return Objects.requireNonNull(rateFactory.createNew());
    }

    public RateLimiter<K> getParent() {
        return parent;
    }

    public Logic getLogic() {
        return logic;
    }

    public Rate[] getLimits() {
        return limits;
    }

    public RateCache<Object> getCache() {
        return cache;
    }

    public RateFactory getRateFactory() {
        return rateFactory;
    }

    public RateExceededListener getRateExceededListener() {
        return rateExceededListener;
    }

    @Override
    public String toString() {
        return "DefaultRateLimiter{" +
                "logic=" + logic +
                ", limits=" + Arrays.toString(limits) +
                '}';
    }
}
