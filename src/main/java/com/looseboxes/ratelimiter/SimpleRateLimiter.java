package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.InMemoryRateCache;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.*;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class SimpleRateLimiter<K extends Serializable> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(SimpleRateLimiter.class);

    private final RateCache<K, Serializable> rateCache;

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final RateFactory rateFactory;

    private final Logic logic;

    private final Rate [] limits;

    private final RateExceededListener rateExceededListener;

    public SimpleRateLimiter(RateConfig rateConfig) {
        this(new RateLimitConfig().addLimit(rateConfig));
    }

    public SimpleRateLimiter(Collection<RateConfig> rateConfig) {
        this(new RateLimitConfig().addLimits(rateConfig));
    }

    public SimpleRateLimiter(RateLimitConfig rateLimitConfig) {
        this(new InMemoryRateCache<>(), new LimitWithinDurationFactory(),
                new RateExceededExceptionThrower(), rateLimitConfig);
    }

    public SimpleRateLimiter(RateCache<K, ? extends Serializable> rateCache, RateFactory rateFactory,
                             RateExceededListener rateExceededListener, RateLimitConfig rateLimitConfig) {
        this(rateCache, rateFactory, rateExceededListener, rateLimitConfig.getLogic(), rateLimitConfig.toRateList().toArray(new Rate[0]));
    }

    public SimpleRateLimiter(RateCache<K, ? extends Serializable> rateCache, RateFactory rateFactory,
                             RateExceededListener rateExceededListener, Logic logic, Rate [] limits) {
        this.rateCache = (RateCache<K, Serializable>)Objects.requireNonNull(rateCache);
        this.rateFactory = Objects.requireNonNull(rateFactory);
        this.rateExceededListener = Objects.requireNonNull(rateExceededListener);
        this.logic = Objects.requireNonNull(logic);
        this.limits = new Rate[limits.length];
        System.arraycopy(limits, 0, this.limits, 0, limits.length);
    }

    public SimpleRateLimiter<K> withRateCache(RateCache<K, ? extends Serializable> rateCache) {
        return new SimpleRateLimiter<K>(rateCache, this.rateFactory, this.rateExceededListener, this.logic, this.limits);
    }

    public SimpleRateLimiter<K> withRateFactory(RateFactory rateFactory) {
        return new SimpleRateLimiter<K>(this.rateCache, rateFactory, this.rateExceededListener, this.logic, this.limits);
    }

    public SimpleRateLimiter<K> withRateExceededListener(RateExceededListener rateExceededListener) {
        return new SimpleRateLimiter<K>(this.rateCache, this.rateFactory, rateExceededListener, this.logic, this.limits);
    }

    public SimpleRateLimiter<K> withRateLimitConfig(RateLimitConfig rateLimitConfig) {
        return new SimpleRateLimiter<K>(this.rateCache, this.rateFactory, rateExceededListener, rateLimitConfig);
    }

    @Override
    public void increment(K key, int amount) {

        Rate firstExceededLimit = null;

        final Rate existingRate = getRateFromCache(key);

        final Rate next = existingRate == null ? newInitialRate() : existingRate.increment(amount);

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

        if(LOG.isDebugEnabled()) {
            LOG.debug("For: {}, limit exceeded: {}, rate: {}, limits: {}",
                    key, firstExceededLimit != null, next, Arrays.toString(limits));
        }

        final Rate result = reset ? newInitialRate() : next;
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
            return (Rate)rateCache.get(key);
        }finally {
            cacheLock.readLock().unlock();
        }
    }

    private void addRateToCache(K key, Rate rate, boolean onlyIfAbsent) {
        try {
            cacheLock.writeLock().lock();
            if (onlyIfAbsent) {
                // This should mitigate different threads attempting to put a new rate, at the same time
                rateCache.putIfAbsent(key, rate);
            } else {
                rateCache.put(key, rate);
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

    protected Rate newInitialRate() {
        return Objects.requireNonNull(rateFactory.createNew());
    }

    public Logic getLogic() {
        return logic;
    }

    public Rate[] getLimits() {
        return limits;
    }

    public RateCache<K, ? extends Serializable> getRateCache() {
        return rateCache;
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
