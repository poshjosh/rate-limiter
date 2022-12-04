package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.rates.Limit;
import com.looseboxes.ratelimiter.RateRecordedListener;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.rates.Rate;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.BucketConfiguration;
import io.github.bucket4j.grid.ProxyManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.util.*;
import java.util.function.Supplier;

/**
 * RateLimiter implementation based on bucket4j
 * @see <a href="https://github.com/vladimir-bukhtoyarov/bucket4j/blob/6.4/doc-pages/jcache-usage.md">bucket4j jcache usage</a>
 * @param <K> The type of the key which the {@link Bucket4jRateLimiter#consume(Object)}} method accepts.
 */
public class Bucket4jRateLimiter<K extends Serializable> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(Bucket4jRateLimiter.class);

    private final ProxyManager<K> buckets;
    private final Limit limit;
    private final Supplier<BucketConfiguration>[] configurationSuppliers;
    private final RateRecordedListener rateRecordedListener;

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, Rate... rates) {
        this(proxyManager, Limit.of(rates));
    }

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, Limit limit) {
        this(proxyManager, BucketConfigurationProvider.simple(), RateRecordedListener.NO_OP, limit);
    }

    public Bucket4jRateLimiter(
            ProxyManager<K> proxyManager,
            BucketConfigurationProvider bucketConfigurationProvider,
            RateRecordedListener rateRecordedListener,
            Limit limit) {
        this.buckets = Objects.requireNonNull(proxyManager);
        this.limit = Objects.requireNonNull(limit);
        this.configurationSuppliers = new Supplier[limit.getRateCount()];
        for(int i = 0; i < limit.getRateCount(); i++) {
            Rate rate = limit.getRates()[i];
            BucketConfiguration configuration = bucketConfigurationProvider.getBucketConfiguration(rate);
            this.configurationSuppliers[i] = () -> configuration;
        }
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
    }

    @Override
    public boolean consume(Object context, K resourceId, int amount) {

        int failCount = 0;

        Bucket firstExceeded = null;

        for (int i = 0; i < configurationSuppliers.length; i++) {

            Bucket bucket = buckets.getProxy(resourceId, configurationSuppliers[i]);

            if(!bucket.tryConsume(amount)) {
                if (firstExceeded == null) {
                    firstExceeded = bucket;
                }
                ++failCount;
            }
        }

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, limit: {}", limit.isExceeded(failCount), resourceId, limit);
        }

        rateRecordedListener.onRateRecorded(context, resourceId, amount, limit, firstExceeded);

        if(limit.isExceeded(failCount)) {

            rateRecordedListener.onRateExceeded(context, resourceId, amount, limit, firstExceeded);

            return false;

        }else{

            return true;
        }
    }

    @Override
    public String toString() {
        return "Bucket4jRateLimiter@" + Integer.toHexString(hashCode()) + limit;
    }
}
