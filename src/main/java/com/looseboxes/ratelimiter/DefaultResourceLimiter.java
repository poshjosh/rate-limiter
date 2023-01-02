package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.cache.RateCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

final class DefaultResourceLimiter<R> implements ResourceLimiter<R> {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultResourceLimiter.class);

    private final ReadWriteLock cacheLock = new ReentrantReadWriteLock();

    private final ResourceLimiterConfig<R, Object> config;

    private final Bandwidths limits;

    DefaultResourceLimiter(ResourceLimiterConfig<R, Object> resourceLimiterConfig, Bandwidths limits) {
        this.config = Objects.requireNonNull(resourceLimiterConfig);
        this.limits = Bandwidths.of(limits);
    }

    @Override
    public boolean tryConsume(R resource, int permits, long timeout, TimeUnit unit) {

        final Object resourceId = config.getKeyProvider().get(resource);

        final Bandwidths existingBandwidths = getBandwidthsFromCache(resourceId);

        final Bandwidths targetBandwidths = existingBandwidths == null 
                ? config.getRateLimiterProvider().provideBandwidths(resourceId, limits) 
                : existingBandwidths;

        RateLimiter rateLimiter = config.getRateLimiterProvider()
                .provideRateLimiter(resourceId, targetBandwidths);

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

        config.getUsageListener().onConsumed(resource, resourceId, permits, targetBandwidths);

        if (acquired) {
            return true;
        }

        config.getUsageListener().onRejected(resource, resourceId, permits, targetBandwidths);

        return false;
    }

    private Bandwidths getBandwidthsFromCache(Object key) {
        try{
            cacheLock.readLock().lock();
            return (Bandwidths) config.getCache().get(key);
        }finally {
            cacheLock.readLock().unlock();
        }
    }

    private void addBandwidthsToCache(Object key, Bandwidths bandwidths, boolean onlyIfAbsent) {
        try {
            cacheLock.writeLock().lock();
            if (onlyIfAbsent) {
                // This should mitigate different threads attempting to put a new rate, at the same time
                config.getCache().putIfAbsent(key, bandwidths);
            } else {
                config.getCache().put(key, bandwidths);
            }
        }finally {
            cacheLock.writeLock().unlock();
        }
    }

    @Override
    public <K> ResourceLimiter<R> keyProvider(KeyProvider<R, K> keyProvider) {
        return new DefaultResourceLimiter<>(
                ResourceLimiterConfig.of(config).keyProvider((KeyProvider) keyProvider), limits);
    }

    @Override public <K> ResourceLimiter<R> cache(RateCache<K, Bandwidths> cache) {
        return new DefaultResourceLimiter<>(
                ResourceLimiterConfig.of(config).cache((RateCache)cache), limits);
    }

    @Override public ResourceLimiter<R> listener(ResourceUsageListener listener) {
        return new DefaultResourceLimiter<>(ResourceLimiterConfig.of(config).usageListener(listener), limits);
    }


    @Override
    public String toString() {
        return "DefaultResourceLimiter@" + Integer.toHexString(hashCode()) + "{" + limits + "}";
    }
}
