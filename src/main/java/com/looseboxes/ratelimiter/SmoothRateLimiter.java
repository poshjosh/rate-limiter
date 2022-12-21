package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.SleepingTicker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class SmoothRateLimiter<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(SmoothRateLimiter.class);

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final RateCache<K, Object> rateCache;

    private final RateRecordedListener rateRecordedListener;

    private final BandwidthLimiterProvider<K> bandwidthLimiterProvider;

    private final Bandwidths defaultBandwidths;

    public SmoothRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Bandwidths bandwidths) {
        this.rateCache = (RateCache<K, Object>)Objects.requireNonNull(rateLimiterConfig.getRateCache());
        this.rateRecordedListener = Objects.requireNonNull(rateLimiterConfig.getRateRecordedListener());
        this.bandwidthLimiterProvider = Objects.requireNonNull(rateLimiterConfig.getBandwidthLimiterProvider());
        this.defaultBandwidths = Objects.requireNonNull(bandwidths);
    }

    @Override
    public boolean tryConsume(Object context, K resourceId, int permits, long timeout, TimeUnit unit) {

        final Bandwidths existingBandwidths = getBandwidthsFromCache(resourceId);

        final Bandwidths targetBandwidths;

        final SleepingTicker ticker = bandwidthLimiterProvider.getTicker(resourceId);
        if (existingBandwidths == null) {
            targetBandwidths = newInitialRate(ticker);
        } else {
            targetBandwidths = existingBandwidths;
        }

        BandwidthLimiter limiter = bandwidthLimiterProvider.getBandwidthLimiter(resourceId, targetBandwidths);

        final boolean acquired = limiter.tryAcquire(permits, timeout, unit);

        if(acquired || existingBandwidths == null) {
            // Initial foray to Cache for this resourceId
            // This should mitigate different threads attempting to put a new rate, at the same time
            final boolean putOnlyIfAbsent = existingBandwidths == null;
            addBandwidthsToCache(resourceId, targetBandwidths, putOnlyIfAbsent);
        }

        //System.out.printf("%s SmoothRateLimiter limit exceeded: %b, for: %s, limit: %s\n",
        //        java.time.LocalTime.now(), !acquired, resourceId, targetBandwidths);

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, limit: {}", !acquired, resourceId, targetBandwidths);
        }

        rateRecordedListener.onRateRecorded(context, resourceId, permits, targetBandwidths);

        if (acquired) {
            return true;
        }

        rateRecordedListener.onRateExceeded(context, resourceId, permits, targetBandwidths);

        return false;
    }

    private Bandwidths getBandwidthsFromCache(K key) {
        try{
            cacheLock.readLock().lock();
            return (Bandwidths) rateCache.get(key);
        }finally {
            cacheLock.readLock().unlock();
        }
    }

    private void addBandwidthsToCache(K key, Bandwidths bandwidths, boolean onlyIfAbsent) {
        try {
            cacheLock.writeLock().lock();
            if (onlyIfAbsent) {
                // This should mitigate different threads attempting to put a new rate, at the same time
                rateCache.putIfAbsent(key, bandwidths);
            } else {
                rateCache.put(key, bandwidths);
            }
        }finally {
            cacheLock.writeLock().unlock();
        }
    }

    private Bandwidths newInitialRate(SleepingTicker ticker) {
        return Bandwidths.copyOf(defaultBandwidths, ticker.elapsedMicros());
    }

    @Override
    public String toString() {
        return "SmoothRateLimiter@" + Integer.toHexString(hashCode()) + '_' + defaultBandwidths;
    }
}
