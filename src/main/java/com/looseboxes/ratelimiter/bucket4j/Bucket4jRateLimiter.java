package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.Limit;
import com.looseboxes.ratelimiter.RateRecordedListener;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.BucketConfiguration;
import io.github.bucket4j.grid.ProxyManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.time.Duration;
import java.util.*;
import java.util.function.Supplier;

/**
 * RateLimiter implementation based on bucket4j
 * @see <a href="https://github.com/vladimir-bukhtoyarov/bucket4j/blob/6.4/doc-pages/jcache-usage.md">bucket4j jcache usage</a>
 * @param <K> The type of the key which the {@link Bucket4jRateLimiter#increment(Object)}} method accepts.
 */
public class Bucket4jRateLimiter<K extends Serializable> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(Bucket4jRateLimiter.class);

    private final ProxyManager<K> buckets;
    private final Limit limit;
    private final Supplier<BucketConfiguration>[] configurationSuppliers;
    private final RateRecordedListener rateRecordedListener;

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, RateRecordedListener rateRecordedListener, Rate... rates) {
        this(proxyManager, rateRecordedListener, Limit.of(rates));
    }

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, RateRecordedListener rateRecordedListener, Limit limit) {
        this.buckets = Objects.requireNonNull(proxyManager);
        this.limit = Objects.requireNonNull(limit);
        this.configurationSuppliers = new Supplier[limit.getRateCount()];
        for(int i = 0; i < limit.getRateCount(); i++) {
            Rate rate = limit.getRates()[i];
            BucketConfiguration configuration = getSimpleBucketConfiguration(rate);
            this.configurationSuppliers[i] = () -> configuration;
        }
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
    }

    private BucketConfiguration getSimpleBucketConfiguration(Rate rate) {
        final AmountPerDuration amountPerDuration = (AmountPerDuration)rate;
        return Bucket4j.configurationBuilder()
                .addLimit(Bandwidth.simple(amountPerDuration.getAmount(), Duration.ofMillis(amountPerDuration.getDuration())))
                .build();
    }

    @Override
    public boolean increment(Object resource, K resourceId, int amount) {

        int failCount = 0;

        List<Rate> exceededLimits = null; // Initialize only when needed

        for (int i = 0; i < configurationSuppliers.length; i++) {

            Bucket bucket = buckets.getProxy(resourceId, configurationSuppliers[i]);

            if(!bucket.tryConsume(amount)) {

                if(exceededLimits == null) {
                    exceededLimits = new ArrayList<>(limit.getRateCount());
                }
                exceededLimits.add(limit.getRates()[i]);

                ++failCount;
            }
        }

        if(exceededLimits == null){
            exceededLimits = Collections.emptyList();
        }

        if(LOG.isTraceEnabled()) {
            LOG.trace("Limit exceeded: {}, for: {}, exceeded limits: {}, all limits: {}",
                    !exceededLimits.isEmpty(), resourceId, exceededLimits, limit);
        }

        rateRecordedListener.onRateRecorded(resource, resourceId, amount, exceededLimits);

        if(limit.isExceeded(failCount)) {

            rateRecordedListener.onRateExceeded(resource, resourceId, amount, exceededLimits);

            return false;

        }else{

            return true;
        }
    }

    @Override
    public String toString() {
        return Bucket4jRateLimiter.class.getSimpleName() + "@" + Integer.toHexString(hashCode()) + limit;
    }
}
