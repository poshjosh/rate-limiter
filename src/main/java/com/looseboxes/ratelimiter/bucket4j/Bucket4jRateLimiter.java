package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.RateExceededEvent;
import com.looseboxes.ratelimiter.RateExceededListener;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.rates.Logic;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateConfigList;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.Bucket4j;
import io.github.bucket4j.BucketConfiguration;
import io.github.bucket4j.grid.ProxyManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serializable;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
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
    private final RateExceededListener rateExceededListener;

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, RateExceededListener rateExceededListener, RateConfig rateConfig) {
        this(proxyManager, rateExceededListener, new RateConfigList().addLimit(rateConfig));
    }

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, RateExceededListener rateExceededListener, Collection<RateConfig> rateConfigs) {
        this(proxyManager, rateExceededListener, new RateConfigList().addLimits(rateConfigs));
    }

    public Bucket4jRateLimiter(ProxyManager<K> proxyManager, RateExceededListener rateExceededListener, RateConfigList rateLimitConfig) {
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
        this.rateExceededListener = Objects.requireNonNull(rateExceededListener);
    }

    private BucketConfiguration getSimpleBucketConfiguration(RateConfig rateConfig) {
        return Bucket4j.configurationBuilder()
                .addLimit(Bandwidth.simple(rateConfig.getLimit(), getDuration(rateConfig)))
                .build();
    }

    private Duration getDuration(RateConfig rateConfig) {
        return Duration.ofMillis(rateConfig.getTimeUnit().toMillis(rateConfig.getDuration()));
    }

    @Override
    public void increment(K key, int amount) {

        int failCount = 0;

        Rate firstExceededLimit = null;

        for (int i = 0; i < configurationSuppliers.length; i++) {

            Bucket bucket = buckets.getProxy(key, configurationSuppliers[i]);

            if(!bucket.tryConsume(1)) {

                if(firstExceededLimit == null) {
                    firstExceededLimit = limits[i];
                }

                ++failCount;
            }
        }

        if(LOG.isDebugEnabled()) {
            LOG.debug("For: {}, exceeded limit: {}, limits: {}",
                    key, firstExceededLimit != null ? firstExceededLimit : false, Arrays.toString(limits));
        }

        if((Logic.OR.equals(logic) && failCount > 0) ||
                (Logic.AND.equals(logic) && failCount == configurationSuppliers.length)) {

            rateExceededListener.onRateExceeded(new RateExceededEvent(this, key, firstExceededLimit));
        }
    }
}
