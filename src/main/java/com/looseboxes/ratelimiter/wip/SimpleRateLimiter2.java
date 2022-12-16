package com.looseboxes.ratelimiter.wip;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Limit;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.wip.SimpleBandwidthLimiter;
import com.looseboxes.ratelimiter.wip.bandwidth.Bandwidth;
import com.looseboxes.ratelimiter.wip.bandwidth.SmoothBandwidth;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

final class SimpleRateLimiter2<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(SimpleRateLimiter2.class);

    private final RateCache<K, Object> rateCache;

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final RateFactory rateFactory;

    private final Limit limit;

    private final RateRecordedListener rateRecordedListener;

    protected SimpleRateLimiter2(RateLimiterConfig<K, ?> rateLimiterConfig, Limit limit) {
        this(rateLimiterConfig.getRateCache(),
                rateLimiterConfig.getRateFactory(),
                rateLimiterConfig.getRateRecordedListener(),
                limit);
    }

    private SimpleRateLimiter2(RateCache<K, ?> rateCache, RateFactory rateFactory,
            RateRecordedListener rateRecordedListener, Limit limit) {
        this.rateCache = (RateCache<K, Object>)Objects.requireNonNull(rateCache);
        this.rateFactory = Objects.requireNonNull(rateFactory);
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
        this.limit = Objects.requireNonNull(limit);
    }

    @Override
    public boolean consume(Object context, K resourceId, int amount) {

        final Rate existingRate = getRateFromCache(resourceId);

        //final Bandwidth bandwidth = SmoothBandwidth.bursty((AmountPerDuration)existingRate, null);

        final Rate targetRate = existingRate == null ? newInitialRate(amount) : existingRate.increment(amount);

        final int comparison = limit.compareTo(targetRate);

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, rate: {}, limit: {}", comparison == 1, resourceId, targetRate, limit);
        }

        final Rate result = comparison == 0 ? newInitialRate(amount) : targetRate;
        if(existingRate != result) {
            final boolean putOnlyIfAbsent = existingRate == null;
            addRateToCache(resourceId, result, putOnlyIfAbsent);
        }

        rateRecordedListener.onRateRecorded(context, resourceId, amount, limit, targetRate);

        if(comparison == 1) {
            rateRecordedListener.onRateExceeded(context, resourceId, amount, limit, targetRate);
            return false;
        }else{
            return true;
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

    private Rate newInitialRate(int amount) {
        return Objects.requireNonNull(rateFactory.createNew(amount));
    }

    public Limit getLimit() {
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
