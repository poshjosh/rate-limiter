package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.RateExceededListener;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.RateLimiterConfiguration;
import com.looseboxes.ratelimiter.RateLimiterFactory;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.RateConfigList;
import io.github.bucket4j.grid.ProxyManager;

import java.io.Serializable;

public class Bucket4jRateLimiterFactory<K extends Serializable> implements RateLimiterFactory<K> {

    private final ProxyManagerProvider proxyManagerProvider;

    public Bucket4jRateLimiterFactory(ProxyManagerProvider proxyManagerProvider) {
        this.proxyManagerProvider = proxyManagerProvider;
    }

    @Override
    public RateLimiter<K> createRateLimiter(
            RateLimiterConfiguration<K, ?> rateLimiterConfiguration,
            RateConfigList rateConfigList) {

        RateCache<K, ?> rateCache = rateLimiterConfiguration.getRateCache();

        ProxyManager<K> proxyManager = proxyManagerProvider.getProxyManager(rateCache);

        RateExceededListener rateExceededListener = rateLimiterConfiguration.getRateExceededListener();

        return new Bucket4jRateLimiter<>(proxyManager, rateExceededListener, rateConfigList);
    }
}
