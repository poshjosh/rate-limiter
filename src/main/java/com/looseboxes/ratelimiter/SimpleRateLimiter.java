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
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class SimpleRateLimiter<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(SimpleRateLimiter.class);

    private final RateCache<K> cache;

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final RateFactory rateFactory;

    private final Logic logic;

    private final Rate [] limits;

    private final RateExceededListener rateExceededListener;

    public SimpleRateLimiter(RateConfig rateConfig) {
        this(new RateLimitConfig().addLimit(rateConfig));
    }

    public SimpleRateLimiter(RateLimitConfig rateLimitConfig) {
        this(new RateLimiterConfiguration<K>()
                .rateCache(new InMemoryRateCache<>())
                .rateFactory(new LimitWithinDurationFactory())
                .rateExceededListener(new RateExceededExceptionThrower()), rateLimitConfig);
    }

    public SimpleRateLimiter(RateLimiterConfiguration<K> rateLimiterConfiguration, RateLimitConfig rateLimitConfig) {
        this.cache = Objects.requireNonNull(rateLimiterConfiguration.getRateCache());
        this.rateFactory = Objects.requireNonNull(rateLimiterConfiguration.getRateFactory());
        this.rateExceededListener = Objects.requireNonNull(rateLimiterConfiguration.getRateExceededListener());
        this.logic = Objects.requireNonNull(rateLimitConfig.getLogic());
        this.limits = rateLimitConfig.toRateList().toArray(new Rate[0]);
    }

    @Override
    public void increment(K key, int amount) {

        Rate firstExceededLimit = null;

        final Rate existingRate = getRateFromCache(key);

        final Rate next = existingRate == null ? getInitialRate() : existingRate.increment(amount);

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
            if((isAnd() && resetCount == limits.length) || (isOr() && resetCount > 0)) {
                reset = true;
            }
        }

        if(LOG.isInfoEnabled()) {
            LOG.info("For: {}, limit exceeded: {}, rate: {}, limits: {}",
                    key, firstExceededLimit != null, next, Arrays.toString(limits));
        }

        final Rate result = reset ? getInitialRate() : next;
        if(existingRate != result) {
            final boolean putOnlyIfAbsent = existingRate == null;
            addRateToCache(key, result, putOnlyIfAbsent);
        }

        if(firstExceededLimit != null) {
            rateExceededListener.onRateExceeded(new RateExceededEvent(this, key, firstExceededLimit));
        }
    }

    private Rate getRateFromCache(K key) {
        try{
            cacheLock.readLock().lock();
            return cache.get(key);
        }finally {
            cacheLock.readLock().unlock();
        }
    }

    private void addRateToCache(K key, Rate rate, boolean onlyIfAbsent) {
        try {
            cacheLock.writeLock().lock();
            if (onlyIfAbsent) {
                // This should mitigate different threads attempting to put a new rate, at the same time
                cache.putIfAbsent(key, rate);
            } else {
                cache.put(key, rate);
            }
        }finally {
            cacheLock.writeLock().unlock();
        }
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

    public Logic getLogic() {
        return logic;
    }

    public Rate[] getLimits() {
        return limits;
    }

    public RateCache<K> getCache() {
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
        return "SimpleRateLimiter@" + Integer.toHexString(hashCode()) + "{logic=" + logic + ", limits=" + Arrays.toString(limits) + '}';
    }
}
