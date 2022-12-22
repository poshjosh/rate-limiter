package com.looseboxes.ratelimiter.bucket4j;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import io.github.bucket4j.grid.ProxyManager;

import java.io.Serializable;
import java.util.Objects;

public class Bucket4jRateLimiterFactory<K extends Serializable> implements RateLimiterFactory<K> {

    private final ProxyManagerProvider proxyManagerProvider;
    private final BucketConfigurationProvider bucketConfigurationProvider;

    public Bucket4jRateLimiterFactory(ProxyManagerProvider proxyManagerProvider) {
        this(proxyManagerProvider, BucketConfigurationProvider.simple());
    }

    public Bucket4jRateLimiterFactory(
            ProxyManagerProvider proxyManagerProvider,
            BucketConfigurationProvider bucketConfigurationProvider) {
        this.proxyManagerProvider = Objects.requireNonNull(proxyManagerProvider);
        this.bucketConfigurationProvider = Objects.requireNonNull(bucketConfigurationProvider);
    }

    @Override
    public RateLimiter<K> createRateLimiter(RateLimiterConfig<K, ?> rateLimiterConfig, Bandwidths bandwidths) {

        RateCache<K, ?> rateCache = rateLimiterConfig.getRateCache();

        ProxyManager<K> proxyManager = proxyManagerProvider.getProxyManager(rateCache);

        RateRecordedListener rateRecordedListener = rateLimiterConfig.getRateRecordedListener();

        return new Bucket4jRateLimiter<>(proxyManager, bucketConfigurationProvider, rateRecordedListener, bandwidths);
    }
}
