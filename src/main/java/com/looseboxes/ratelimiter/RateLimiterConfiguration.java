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
        this.setRateRecordedListener(rateLimiterConfiguration.rateExceededListener);
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

    public RateLimiterConfiguration<K> rateRecordedListener(
            RateExceededListener rateExceededListener) {
        this.setRateRecordedListener(rateExceededListener);
        return this;
    }

    public RateExceededListener getRateRecordedListener() {
        return rateExceededListener;
    }

    public void setRateRecordedListener(RateExceededListener rateExceededListener) {
        this.rateExceededListener = rateExceededListener;
    }
}
