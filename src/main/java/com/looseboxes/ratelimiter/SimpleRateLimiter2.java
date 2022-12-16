package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.rates.Rate;
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

    private final RateLimiter rateLimiter;

    private final RateRecordedListener rateRecordedListener;

    protected SimpleRateLimiter2(RateLimiterConfig<K, ?> rateLimiterConfig, RateLimiter rateLimiter) {
        this(rateLimiterConfig.getRateCache(),
                rateLimiterConfig.getRateFactory(),
                rateLimiterConfig.getRateRecordedListener(),
                rateLimiter);
    }

    private SimpleRateLimiter2(RateCache<K, ?> rateCache, RateFactory rateFactory,
                             RateRecordedListener rateRecordedListener, RateLimiter rateLimiter) {
        this.rateCache = (RateCache<K, Object>)Objects.requireNonNull(rateCache);
        this.rateFactory = Objects.requireNonNull(rateFactory);
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
        this.rateLimiter = Objects.requireNonNull(rateLimiter);
    }

    @Override
    public boolean consume(Object context, K resourceId, int amount) {

        Rate existingRate = getRateFromCache(resourceId);

        if (existingRate == null) {
            existingRate = newInitialRate(amount);
        }

        Rate result = existingRate;

        final boolean success = rateLimiter.consume(context, result, amount);

        if(existingRate != result) {
            final boolean putOnlyIfAbsent = existingRate == null;
            addRateToCache(resourceId, result, putOnlyIfAbsent);
        }

        com.looseboxes.ratelimiter.rates.Limit limit = null; // TODO

        rateRecordedListener.onRateRecorded(context, resourceId, amount, limit, result);

        if(!success) {
            rateRecordedListener.onRateExceeded(context, resourceId, amount, limit, result);
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

    public RateCache<K, ?> getRateCache() {
        return rateCache;
    }

    public RateFactory getRateFactory() {
        return rateFactory;
    }

    public RateRecordedListener getRateRecordedListener() {
        return rateRecordedListener;
    }
}
