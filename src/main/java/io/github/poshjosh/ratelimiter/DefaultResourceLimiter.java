package io.github.poshjosh.ratelimiter;

import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;
import io.github.poshjosh.ratelimiter.cache.RateCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

final class DefaultResourceLimiter<R> implements ResourceLimiter<R> {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultResourceLimiter.class);

    private final Bandwidths limits;

    private final SleepingTicker ticker;

    private final UsageListener listener;

    private final RateCache<R> cache;

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final Map<Object, RateLimiter> resourceIdToRateLimiters;

    DefaultResourceLimiter(
            Bandwidths limits, SleepingTicker ticker,
            UsageListener listener, RateCache<R> cache) {
        this.limits = Bandwidths.of(limits);
        this.ticker = Objects.requireNonNull(ticker);
        this.listener = Objects.requireNonNull(listener);
        this.cache = Objects.requireNonNull(cache);
        this.resourceIdToRateLimiters = new ConcurrentHashMap<>();
    }

    @Override
    public boolean tryConsume(R resourceId, int permits, long timeout, TimeUnit unit) {

        final Bandwidths existingBandwidths = getBandwidthsFromCache(resourceId);

        final Bandwidths targetBandwidths = existingBandwidths == null 
                ? provideBandwidths(resourceId, limits)
                : existingBandwidths;

        final RateLimiter rateLimiter = provideRateLimiter(resourceId, targetBandwidths);

        final boolean acquired = rateLimiter.tryAcquire(permits, timeout, unit);

        if(acquired || existingBandwidths == null) {
            // Initial foray to Cache for this resourceId
            // This should mitigate different threads attempting to put a new rate, at the same time
            final boolean putOnlyIfAbsent = existingBandwidths == null;
            addBandwidthsToCache(resourceId, targetBandwidths, putOnlyIfAbsent);
        }

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, limit: {}", !acquired, resourceId, targetBandwidths);
        }

        listener.onConsumed(resourceId, permits, targetBandwidths);

        if (acquired) {
            return true;
        }

        listener.onRejected(resourceId, permits, targetBandwidths);

        return false;
    }

    private Bandwidths getBandwidthsFromCache(R key) {
        try{
            cacheLock.readLock().lock();
            return cache.get(key);
        }finally {
            cacheLock.readLock().unlock();
        }
    }

    private void addBandwidthsToCache(R key, Bandwidths bandwidths, boolean onlyIfAbsent) {
        try {
            cacheLock.writeLock().lock();
            if (onlyIfAbsent) {
                // This should mitigate different threads attempting to put a new rate, at the same time
                cache.putIfAbsent(key, bandwidths);
            } else {
                cache.put(key, bandwidths);
            }
        }finally {
            cacheLock.writeLock().unlock();
        }
    }

    private Bandwidths provideBandwidths(R key, Bandwidths bandwidths) {
        return bandwidths.with(ticker.elapsedMicros());
    }

    private RateLimiter provideRateLimiter(R key, Bandwidths bandwidths) {
        RateLimiter value;
        if ((value = this.resourceIdToRateLimiters.get(key)) == null) {
            RateLimiter newValue;
            if ((newValue = createNew(bandwidths)) != null) {
                this.resourceIdToRateLimiters.put(key, newValue);
                return newValue;
            }
        }
        return value;
    }

    private RateLimiter createNew(Bandwidths bandwidths) {
        return RateLimiter.of(bandwidths, ticker);
    }

    @Override
    public String toString() {
        return "DefaultResourceLimiter@" + Integer.toHexString(hashCode()) + "{" + limits + "}";
    }
}
