package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;

import java.io.Serializable;

public class RateLimiterConfiguration<K extends Serializable, V extends Serializable> {

    private RateCache<K, V> rateCache;
    private RateFactory rateFactory;
    private RateExceededListener rateExceededListener;

    public RateLimiterConfiguration() { }

    public RateLimiterConfiguration(RateLimiterConfiguration<K, V> rateLimiterConfiguration) {
        this.setRateCache(rateLimiterConfiguration.rateCache);
        this.setRateFactory(rateLimiterConfiguration.rateFactory);
        this.setRateExceededListener(rateLimiterConfiguration.rateExceededListener);
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
