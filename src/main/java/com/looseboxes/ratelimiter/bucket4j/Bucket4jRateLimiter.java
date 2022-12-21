package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.RateRecordedListener;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.util.SleepingTicker;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.BucketConfiguration;
import io.github.bucket4j.EstimationProbe;
import io.github.bucket4j.grid.ProxyManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

/**
 * RateLimiter implementation based on bucket4j
 * @see <a href="https://github.com/vladimir-bukhtoyarov/bucket4j/blob/6.4/doc-pages/jcache-usage.md">bucket4j jcache usage</a>
 * @param <K> The type of the key which the {@link Bucket4jRateLimiter#tryConsume(Object)}} method accepts.
 */
public class Bucket4jRateLimiter<K extends Serializable> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(Bucket4jRateLimiter.class);

    private final SleepingTicker ticker = SleepingTicker.zeroOffset();

    private final ProxyManager<K> buckets;
    private final Bandwidths limit;
    private final Supplier<BucketConfiguration>[] configurationSuppliers;
    private final RateRecordedListener rateRecordedListener;

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, Bandwidth... bandwidths) {
        this(proxyManager, Bandwidths.of(bandwidths));
    }

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, Bandwidths bandwidths) {
        this(proxyManager, BucketConfigurationProvider.simple(), RateRecordedListener.NO_OP, bandwidths);
    }

    public Bucket4jRateLimiter(
            ProxyManager<K> proxyManager,
            BucketConfigurationProvider bucketConfigurationProvider,
            RateRecordedListener rateRecordedListener,
            Bandwidths bandwidths) {
        this.buckets = Objects.requireNonNull(proxyManager);
        this.limit = Objects.requireNonNull(bandwidths);
        final Bandwidth [] members = bandwidths.getMembers();
        this.configurationSuppliers = new Supplier[members.length];
        for(int i = 0; i < members.length; i++) {
            BucketConfiguration configuration = bucketConfigurationProvider.getBucketConfiguration(members[i]);
            this.configurationSuppliers[i] = () -> configuration;
        }
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
    }

    @Override
    public boolean tryConsume(Object context, K resourceId, int amount, long timeout, TimeUnit unit) {

        int failCount = 0;

        for (Supplier<BucketConfiguration> configurationSupplier : configurationSuppliers) {

            Bucket bucket = buckets.getProxy(resourceId, configurationSupplier);

            if (tryAcquire(bucket, amount, timeout, unit)) {
                continue;
            }

            ++failCount;
        }

        final boolean limitExceeded = limit.isExceeded(failCount);

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, failures: {}/{}, limit: {}",
                    limitExceeded, resourceId, failCount, configurationSuppliers.length, limit);
        }

        rateRecordedListener.onRateRecorded(context, resourceId, amount, limit);

        if (!limitExceeded) {
            return true;
        }

        rateRecordedListener.onRateExceeded(context, resourceId, amount, limit);

        return false;
    }

    private boolean tryAcquire(Bucket bucket, int permits, long timeout, TimeUnit unit) {
        EstimationProbe estimate = bucket.estimateAbilityToConsume(permits);
        if (!estimate.canBeConsumed()) {
            final long nanosToWait = estimate.getNanosToWaitForRefill();
            final long timeoutNanos = unit.toNanos(timeout);
            if (nanosToWait > timeoutNanos) {
                return false;
            }
            sleepNanosUninterruptibly(nanosToWait);
        }
        bucket.tryConsume(permits);
        return true;
    }

    private void sleepNanosUninterruptibly(long nanosToWait) {
        ticker.sleepMicrosUninterruptibly(TimeUnit.NANOSECONDS.toMicros(nanosToWait));
    }

    @Override
    public String toString() {
        return "Bucket4jRateLimiter@" + Integer.toHexString(hashCode()) + limit;
    }
}
