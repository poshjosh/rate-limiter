package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.RateLimitConfig;

public class RateLimiterConfiguration<K> {

    private RateCache<K> rateCache;
    private RateSupplier rateSupplier;
    private RateExceededHandler rateExceededHandler;
    private RateLimitConfig rateLimitConfig;

    public RateLimiterConfiguration() { }

    public RateLimiterConfiguration(RateLimiterConfiguration<K> rateLimiterConfiguration) {
        this.setRateCache(rateLimiterConfiguration.rateCache);
        this.setRateSupplier(rateLimiterConfiguration.rateSupplier);
        this.setRateExceededHandler(rateLimiterConfiguration.rateExceededHandler);
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

    public RateLimiterConfiguration<K> rateSupplier(RateSupplier rateSupplier) {
        this.setRateSupplier(rateSupplier);
        return this;
    }

    public RateSupplier getRateSupplier() {
        return rateSupplier;
    }

    public void setRateSupplier(RateSupplier rateSupplier) {
        this.rateSupplier = rateSupplier;
    }

    public RateLimiterConfiguration<K> rateExceededHandler(RateExceededHandler rateExceededHandler) {
        this.setRateExceededHandler(rateExceededHandler);
        return this;
    }

    public RateExceededHandler getRateExceededHandler() {
        return rateExceededHandler;
    }

    public void setRateExceededHandler(RateExceededHandler rateExceededHandler) {
        this.rateExceededHandler = rateExceededHandler;
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
