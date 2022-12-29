package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.cache.RateCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

final class DefaultResourceLimiter<K> implements ResourceLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultResourceLimiter.class);

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final RateCache<K, Object> rateCache;

    private final ResourceUsageListener resourceUsageListener;

    private final RateLimiterProvider<K> rateLimiterProvider;

    private final Bandwidths limits;

    DefaultResourceLimiter(ResourceLimiterConfig<K, ?> resourceLimiterConfig, Bandwidths limits) {
        this.rateCache = (RateCache<K, Object>)Objects.requireNonNull(resourceLimiterConfig.getCache());
        this.resourceUsageListener = Objects.requireNonNull(resourceLimiterConfig.getUsageListener());
        this.rateLimiterProvider = Objects.requireNonNull(resourceLimiterConfig.getRateLimiterProvider());
        this.limits = Bandwidths.of(limits);
    }

    @Override
    public boolean tryConsume(Object context, K resourceId, int permits, long timeout, TimeUnit unit) {

        final Bandwidths existingBandwidths = getBandwidthsFromCache(resourceId);

        final Bandwidths targetBandwidths = existingBandwidths == null ? createBandwidths() : existingBandwidths;

        RateLimiter rateLimiter = rateLimiterProvider.provideRateLimiter(resourceId, targetBandwidths);

        final boolean acquired = rateLimiter.tryAcquire(permits, timeout, unit);

        if(acquired || existingBandwidths == null) {
            // Initial foray to Cache for this resourceId
            // This should mitigate different threads attempting to put a new rate, at the same time
            final boolean putOnlyIfAbsent = existingBandwidths == null;
            addBandwidthsToCache(resourceId, targetBandwidths, putOnlyIfAbsent);
        }

        //System.out.printf("%s DefaultResourceLimiter limit exceeded: %b, for: %s, limit: %s\n",
        //        java.time.LocalTime.now(), !acquired, resourceId, targetBandwidths);

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, limit: {}", !acquired, resourceId, targetBandwidths);
        }

        resourceUsageListener.onConsumed(context, resourceId, permits, targetBandwidths);

        if (acquired) {
            return true;
        }

        resourceUsageListener.onRejected(context, resourceId, permits, targetBandwidths);

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

    private Bandwidths createBandwidths() {
        return rateLimiterProvider.initFrom(limits);
    }

    @Override
    public String toString() {
        return "DefaultResourceLimiter@" + Integer.toHexString(hashCode()) + "{" + limits + "}";
    }
}
