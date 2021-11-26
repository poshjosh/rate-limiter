package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

public class RateLimiterConfiguration<K> {

    private RateCache<K> rateCache;
    private RateFactory rateFactory;
    private RateRecordedListener rateRecordedListener;
    private RateLimitConfig rateLimitConfig;

    public RateLimiterConfiguration() { }

    public RateLimiterConfiguration(RateLimiterConfiguration<K> rateLimiterConfiguration) {
        this.setRateCache(rateLimiterConfiguration.rateCache);
        this.setRateFactory(rateLimiterConfiguration.rateFactory);
        this.setRateRecordedListener(rateLimiterConfiguration.rateRecordedListener);
        this.setRateLimitConfig(rateLimiterConfiguration.rateLimitConfig == null ? null : new RateLimitConfig(rateLimiterConfiguration.rateLimitConfig));
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

    public RateLimiterConfiguration<K> rateRecordedListener(RateRecordedListener rateRecordedListener) {
        this.setRateRecordedListener(rateRecordedListener);
        return this;
    }

    public RateRecordedListener getRateRecordedListener() {
        return rateRecordedListener;
    }

    public void setRateRecordedListener(RateRecordedListener rateRecordedListener) {
        this.rateRecordedListener = rateRecordedListener;
    }

    public RateLimiterConfiguration<K> rateLimitConfig(RateLimitConfig rateLimitConfig) {
        this.setRateLimitConfig(rateLimitConfig);
        return this;
    }

    public RateLimitConfig getRateLimitConfig() {
        return rateLimitConfig;
    }

    public void setRateLimitConfig(RateLimitConfig rateLimitConfig) {
        this.rateLimitConfig = rateLimitConfig;
    }
}
