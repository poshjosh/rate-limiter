package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.MapRateCache;
import com.looseboxes.ratelimiter.cache.RateCache;

import java.util.Objects;

public class RateLimiterConfiguration<K, V> {

    private RateCache<K, V> rateCache;
    private RateFactory rateFactory;
    private RateExceededListener rateExceededListener;

    public RateLimiterConfiguration() {
        this(new MapRateCache<>(), new LimitWithinDurationFactory(), new RateExceededExceptionThrower());
    }

    public RateLimiterConfiguration(RateLimiterConfiguration<K, V> rateLimiterConfiguration) {
        this(rateLimiterConfiguration.rateCache, rateLimiterConfiguration.rateFactory, rateLimiterConfiguration.rateExceededListener);
    }

    public RateLimiterConfiguration(RateCache<K, V> rateCache, RateFactory rateFactory, RateExceededListener rateExceededListener) {
        this.rateCache = Objects.requireNonNull(rateCache);
        this.rateFactory = Objects.requireNonNull(rateFactory);
        this.rateExceededListener = Objects.requireNonNull(rateExceededListener);
    }

    public RateLimiterConfiguration<K, V> rateCache(RateCache<K, V> rateCache) {
        this.setRateCache(rateCache);
        return this;
    }

    public RateCache<K, V> getRateCache() {
        return rateCache;
    }

    public void setRateCache(RateCache<K, V> rateCache) {
        this.rateCache = rateCache;
    }

    public RateLimiterConfiguration<K, V> rateFactory(RateFactory rateFactory) {
        this.setRateFactory(rateFactory);
        return this;
    }

    public RateFactory getRateFactory() {
        return rateFactory;
    }

    public void setRateFactory(RateFactory rateFactory) {
        this.rateFactory = rateFactory;
    }

    public RateLimiterConfiguration<K, V> rateExceededListener(
            RateExceededListener rateExceededListener) {
        this.setRateExceededListener(rateExceededListener);
        return this;
    }

    public RateExceededListener getRateExceededListener() {
        return rateExceededListener;
    }

    public void setRateExceededListener(RateExceededListener rateExceededListener) {
        this.rateExceededListener = rateExceededListener;
    }
}
