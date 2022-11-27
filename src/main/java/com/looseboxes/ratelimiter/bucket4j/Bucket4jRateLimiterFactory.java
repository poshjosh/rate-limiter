package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.RateCache;
import io.github.bucket4j.grid.ProxyManager;

import java.io.Serializable;

public class Bucket4jRateLimiterFactory<K extends Serializable> implements RateLimiterFactory<K> {

    private final ProxyManagerProvider proxyManagerProvider;

    public Bucket4jRateLimiterFactory(ProxyManagerProvider proxyManagerProvider) {
        this.proxyManagerProvider = proxyManagerProvider;
    }

    @Override
    public RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Limit limit) {

        RateCache<K, ?> rateCache = rateLimiterConfig.getRateCache();

        ProxyManager<K> proxyManager = proxyManagerProvider.getProxyManager(rateCache);

        RateRecordedListener rateRecordedListener = rateLimiterConfig.getRateRecordedListener();

        return new Bucket4jRateLimiter<>(proxyManager, rateRecordedListener, limit);
    }
}
