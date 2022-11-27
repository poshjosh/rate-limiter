package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.MapRateCache;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.rates.Rate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class SimpleRateLimiter<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(SimpleRateLimiter.class);

    private final RateCache<K, Object> rateCache;

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final RateFactory rateFactory;

    private final Limit limit;

    private final RateRecordedListener rateRecordedListener;

    public SimpleRateLimiter(Rate... rates) {
        this(new MapRateCache<>(), new AmountPerDurationFactory(),
                new RateExceededExceptionThrower(), Limit.of(rates));
    }

    public SimpleRateLimiter(RateCache<K, ?> rateCache, RateFactory rateFactory,
                             RateRecordedListener rateRecordedListener, Limit limit) {
        this.rateCache = (RateCache<K, Object>)Objects.requireNonNull(rateCache);
        this.rateFactory = Objects.requireNonNull(rateFactory);
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
        this.limit = Objects.requireNonNull(limit);
    }

    public SimpleRateLimiter<K> withRateCache(RateCache<K, ?> rateCache) {
        return new SimpleRateLimiter<>(rateCache, this.rateFactory, this.rateRecordedListener, this.limit);
    }

    public SimpleRateLimiter<K> withRateFactory(RateFactory rateFactory) {
        return new SimpleRateLimiter<>(this.rateCache, rateFactory, this.rateRecordedListener, this.limit);
    }

    public SimpleRateLimiter<K> withRateRecordedListener(RateRecordedListener rateRecordedListener) {
        return new SimpleRateLimiter<>(this.rateCache, this.rateFactory, rateRecordedListener, this.limit);
    }

    public SimpleRateLimiter<K> withLimit(Limit limit) {
        return new SimpleRateLimiter<>(this.rateCache, this.rateFactory, rateRecordedListener, limit);
    }

    @Override
    public boolean increment(Object resource, K resourceId, int amount) {

        final Rate existingRate = getRateFromCache(resourceId);

        final Rate next = existingRate == null ? newInitialRate() : existingRate.increment(amount);

        int resetCount = 0;
        int failCount = 0;
        List<Rate> exceededLimits = null; // Initialize only when needed

        for(Rate rate : limit.getRates()) {

            final int n = next.compareTo(rate);

            if(n == 0) {

                ++resetCount;

            }else if(n > 0) {

                ++failCount;

                if(exceededLimits == null) {
                    exceededLimits = new ArrayList<>(this.limit.getRateCount());
                }
                exceededLimits.add(rate);
            }
        }

        if(exceededLimits == null){
            exceededLimits = Collections.emptyList();
        }

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, rate: {}, exceeded limits: {}, all limits: {}",
                    !exceededLimits.isEmpty(), resourceId, next, exceededLimits, limit);
        }

        final Rate result = shouldReset(resetCount) ? newInitialRate() : next;
        if(existingRate != result) {
            final boolean putOnlyIfAbsent = existingRate == null;
            addRateToCache(resourceId, result, putOnlyIfAbsent);
        }

        rateRecordedListener.onRateRecorded(resource, resourceId, amount, exceededLimits);

        if(limit.isExceeded(failCount)) {
            rateRecordedListener.onRateExceeded(resource, resourceId, amount, exceededLimits);
            return false;
        }else{
            return true;
        }
    }

    private boolean shouldReset(int resetCount) {
        return limit.isExceeded(resetCount);
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

    private Rate newInitialRate() {
        return Objects.requireNonNull(rateFactory.createNew());
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
