package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;

public class RateLimiterConfiguration<K> {

    private RateCache<K> rateCache;
    private RateFactory rateFactory;
    private RateExceededListener rateExceededListener;

    public RateLimiterConfiguration() { }

    public RateLimiterConfiguration(RateLimiterConfiguration<K> rateLimiterConfiguration) {
        this.setRateCache(rateLimiterConfiguration.rateCache);
        this.setRateFactory(rateLimiterConfiguration.rateFactory);
        this.setRateExceededListener(rateLimiterConfiguration.rateExceededListener);
    }

    public RateLimiterConfiguration<K> rateCache(RateCache<K> rateCache) {
        this.setRateCache(rateCache);
        return this;
    }

    public RateCache<K> getRateCache() {
        return rateCache;
    }

    public void setRateCache(RateCache<K> rateCache) {
        this.rateCache = rateCache;
    }

    public RateLimiterConfiguration<K> rateFactory(RateFactory rateFactory) {
        this.setRateFactory(rateFactory);
        return this;
    }

    public RateFactory getRateFactory() {
        return rateFactory;
    }

    public void setRateFactory(RateFactory rateFactory) {
        this.rateFactory = rateFactory;
    }

    public RateLimiterConfiguration<K> rateExceededListener(
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
