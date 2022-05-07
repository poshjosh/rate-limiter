package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.RateRecordedListener;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateConfigList;
import com.looseboxes.ratelimiter.util.Util;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.Bucket4j;
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
 * @param <K> The type of the key which the {@link Bucket4jRateLimiter#increment(Object)}} method accepts.
 */
public class Bucket4jRateLimiter<K extends Serializable> implements RateLimiter<K> {

    private static final Logger LOG = LoggerFactory.getLogger(Bucket4jRateLimiter.class);

    private final ProxyManager<K> buckets;
    private final Logic logic;
    private final Rate [] limits;
    private final Supplier<BucketConfiguration>[] configurationSuppliers;
    private final RateRecordedListener rateRecordedListener;

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, RateRecordedListener rateRecordedListener, RateConfig rateConfig) {
        this(proxyManager, rateRecordedListener, new RateConfigList().addLimit(rateConfig));
    }

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, RateRecordedListener rateRecordedListener, Collection<RateConfig> rateConfigs) {
        this(proxyManager, rateRecordedListener, new RateConfigList().addLimits(rateConfigs));
    }

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, RateRecordedListener rateRecordedListener, RateConfigList rateLimitConfig) {
        this.buckets = Objects.requireNonNull(proxyManager);
        this.logic = rateLimitConfig.getLogic();
        this.limits = rateLimitConfig.toRateList().toArray(new Rate[0]);
        this.configurationSuppliers = new Supplier[limits.length];
        List<RateConfig> rateConfigList = rateLimitConfig.getLimits();
        for(int i = 0; i < this.limits.length; i++) {
            RateConfig rateConfig = rateConfigList.get(i);
            BucketConfiguration configuration = getSimpleBucketConfiguration(rateConfig);
            this.configurationSuppliers[i] = () -> configuration;
        }
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
    }

    private BucketConfiguration getSimpleBucketConfiguration(RateConfig rateConfig) {
        return Bucket4j.configurationBuilder()
                .addLimit(Bandwidth.simple(rateConfig.getLimit(), rateConfig.getDuration()))
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
                    exceededLimits = new ArrayList<>(limits.length);
                }
                exceededLimits.add(limits[i]);

                ++failCount;
            }
        }

        if(exceededLimits == null){
            exceededLimits = Collections.emptyList();
        }

        if(LOG.isDebugEnabled()) {
            LOG.debug("For: {}, limit exceeded: {}, exceeded limits: {}, all limits: {}",
                    resourceId, !exceededLimits.isEmpty(), exceededLimits, limits);
        }

        rateRecordedListener.onRateRecorded(resource, resourceId, amount, exceededLimits);

        if(Util.isLimitExceeded(failCount, logic, limits)) {

            rateRecordedListener.onRateExceeded(resource, resourceId, amount, exceededLimits);

            return false;

        }else{

            return true;
        }
    }
}
