package com.wip;

import com.looseboxes.ratelimiter.RateFactory;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.RateLimiterConfig;
import com.looseboxes.ratelimiter.RateRecordedListener;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.CompositeRate;
import com.looseboxes.ratelimiter.Rate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

final class SimpleRateLimiter<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(SimpleRateLimiter.class);

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final RateCache<K, Object> rateCache;

    private final RateFactory rateFactory;

    private final RateRecordedListener rateRecordedListener;

    private final CompositeRate limit;

    protected SimpleRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, CompositeRate limit) {
        this(rateLimiterConfig.getRateCache(),
                rateLimiterConfig.getRateFactory(),
                rateLimiterConfig.getRateRecordedListener(),
                limit);
    }

    private SimpleRateLimiter(RateCache<K, ?> rateCache, RateFactory rateFactory,
                             RateRecordedListener rateRecordedListener, CompositeRate limit) {
        this.rateCache = (RateCache<K, Object>)Objects.requireNonNull(rateCache);
        this.rateFactory = Objects.requireNonNull(rateFactory);
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
        this.limit = Objects.requireNonNull(limit);
    }

    @Override
    public boolean tryConsume(Object context, K resourceId, int amount, long timeout, TimeUnit unit) {

        final Rate existingRate = getRateFromCache(resourceId);

        // TODO - Implement increment and uncomment out
        final Rate targetRate = null;//existingRate == null ? newInitialRate(amount) : existingRate.increment(amount);

        final int comparison = limit.compareTo(targetRate);

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, rate: {}, limit: {}", comparison == 1, resourceId, targetRate, limit);
        }

        final Rate result = comparison == 0 ? newInitialRate(amount) : targetRate;
        if(existingRate != result) {
            // Initial foray to Cache for this resourceId
            // This should mitigate different threads attempting to put a new rate, at the same time
            final boolean putOnlyIfAbsent = existingRate == null;
            addRateToCache(resourceId, result, putOnlyIfAbsent);
        }

        rateRecordedListener.onRateRecorded(context, resourceId, amount, limit);

        if (comparison != 1) {
            return true;
        }

        rateRecordedListener.onRateExceeded(context, resourceId, amount, limit);

        return false;
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
                rateCache.putIfAbsent(key, rate);
            } else {
                rateCache.put(key, rate);
            }
        }finally {
            cacheLock.writeLock().unlock();
        }
    }

    private Rate newInitialRate(int amount) {
        return Objects.requireNonNull(rateFactory.createNew(amount));
    }

    public CompositeRate getLimit() {
        return limit;
    }

    public RateCache<K, ?> getRateCache() {
        return rateCache;
    }

    public RateFactory getRateFactory() {
        return rateFactory;
    }

    public RateRecordedListener getRateRecordedListener() {
        return rateRecordedListener;
    }

    @Override
    public String toString() {
        return "SimpleRateLimiter@" + Integer.toHexString(hashCode()) + '_' + limit;
    }
}
