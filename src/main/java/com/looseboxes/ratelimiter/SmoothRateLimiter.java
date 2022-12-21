package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.CompositeRate;
import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.SmoothBandwidth;
import com.looseboxes.ratelimiter.util.SleepingTicker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public abstract class SmoothRateLimiter<K> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(SmoothRateLimiter.class);

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final RateCache<K, Object> rateCache;

    private final Bandwidth [] defaultBandwidths;

    private final RateRecordedListener rateRecordedListener;

    private final CompositeRate limit;

    private final BandwidthLimiterProvider<K> bandwidthLimiterProvider;

    public static <K> SmoothRateLimiter<K> bursty(RateLimiterConfig<K, ?> rateLimiterConfig, CompositeRate limit) {
        return new SmoothRateLimiter<K>(rateLimiterConfig, limit) {
            @Override
            protected Bandwidth toBandwidth(double permitsPerSeconds, long nowMicros) {
                return SmoothBandwidth.bursty(permitsPerSeconds, nowMicros);
            };
        };
    }

    public static <K> SmoothRateLimiter<K> warmingUp(
            RateLimiterConfig<K, ?> rateLimiterConfig, CompositeRate limit, long warmupPeriodSeconds) {
        return new SmoothRateLimiter<K>(rateLimiterConfig, limit) {
            @Override
            protected Bandwidth toBandwidth(double permitsPerSeconds, long nowMicros) {
                return SmoothBandwidth.warmingUp(permitsPerSeconds, nowMicros, warmupPeriodSeconds);
            };
        };
    }

    protected SmoothRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, CompositeRate limit) {
        this.defaultBandwidths = toBandwidths(limit);
        this.rateCache = (RateCache<K, Object>)Objects.requireNonNull(rateLimiterConfig.getRateCache());
        this.limit = Objects.requireNonNull(limit);
        this.rateRecordedListener = Objects.requireNonNull(rateLimiterConfig.getRateRecordedListener());
        this.bandwidthLimiterProvider = Objects.requireNonNull(rateLimiterConfig.getBandwidthLimiterFactory());
    }

    protected Bandwidth [] toBandwidths(CompositeRate limit) {
        Rate [] rates = limit.getRates();
        Bandwidth [] result = new Bandwidth[rates.length];
        for (int i = 0; i < result.length; i++) {
            result[i] = toBandwidth(rates[i].getRateMillis() * 1000, 0);
        }
        return result;
    }

    protected abstract Bandwidth toBandwidth(double permitsPerSeconds, long nowMicros);

    @Override
    public boolean tryConsume(Object context, K resourceId, int permits, long timeout, TimeUnit unit) {

        final Bandwidth [] existingBandwidths = getRateFromCache(resourceId);

        final Bandwidth [] targetBandwidths;

        final SleepingTicker ticker = bandwidthLimiterProvider.getTicker(resourceId);
        if (existingBandwidths == null) {
            targetBandwidths = newInitialRate(ticker);
        } else {
            targetBandwidths = existingBandwidths;
        }

        BandwidthLimiter limiter = bandwidthLimiterProvider
                .getBandwidthLimiter(resourceId, targetBandwidths, limit.getOperator());

        final boolean acquired = limiter.tryAcquire(permits, timeout, unit);

        if(acquired && !Arrays.equals(existingBandwidths, targetBandwidths)) {
            // Initial foray to Cache for this resourceId
            // This should mitigate different threads attempting to put a new rate, at the same time
            final boolean putOnlyIfAbsent = existingBandwidths == null;
            addRateToCache(resourceId, targetBandwidths, putOnlyIfAbsent);
        }

        //System.out.printf("%s SmoothRateLimiter limit exceeded: %b, for: %s, limit: %s\n",
        //        java.time.LocalTime.now(), !acquired, resourceId, limit);

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, limit: {}", !acquired, resourceId, limit);
        }

        rateRecordedListener.onRateRecorded(context, resourceId, permits, limit);

        if (acquired) {
            return true;
        }

        rateRecordedListener.onRateExceeded(context, resourceId, permits, limit);

        return false;
    }

    private Bandwidth[] getRateFromCache(K key) {
        try{
            cacheLock.readLock().lock();
            return (Bandwidth[]) rateCache.get(key);
        }finally {
            cacheLock.readLock().unlock();
        }
    }

    private void addRateToCache(K key, Bandwidth[] rate, boolean onlyIfAbsent) {
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

    private Bandwidth[] newInitialRate(SleepingTicker ticker) {
        Bandwidth [] result = new Bandwidth[defaultBandwidths.length];
        for (int i = 0; i < result.length; i++) {
            result[i] = defaultBandwidths[i].copy(ticker.elapsed(TimeUnit.MICROSECONDS));
        }
        return result;
    }

    public CompositeRate getLimit() {
        return limit;
    }

    public RateCache<K, ?> getRateCache() {
        return rateCache;
    }

    public RateRecordedListener getRateRecordedListener() {
        return rateRecordedListener;
    }

    public BandwidthLimiterProvider<K> getBandwidthLimiterProvider() {
        return bandwidthLimiterProvider;
    }

    @Override
    public String toString() {
        return "SmoothRateLimiter@" + Integer.toHexString(hashCode()) + '_' + limit;
    }
}
