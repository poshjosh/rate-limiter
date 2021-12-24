package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.MapRateCache;
import com.looseboxes.ratelimiter.cache.RateCache;

import java.util.Objects;

public class RateLimiterConfig<K, V> {

    private RateCache<K, V> rateCache;
    private RateFactory rateFactory;
    private RateRecordedListener rateRecordedListener;

    public RateLimiterConfig() {
        this(new MapRateCache<>(), new LimitWithinDurationFactory(), new RateExceededExceptionThrower());
    }

    public RateLimiterConfig(RateLimiterConfig<K, V> rateLimiterConfig) {
        this(rateLimiterConfig.rateCache, rateLimiterConfig.rateFactory, rateLimiterConfig.rateRecordedListener);
    }

    public RateLimiterConfig(RateCache<K, V> rateCache, RateFactory rateFactory, RateRecordedListener rateRecordedListener) {
        this.rateCache = Objects.requireNonNull(rateCache);
        this.rateFactory = Objects.requireNonNull(rateFactory);
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
    }

    public RateLimiterConfig<K, V> rateCache(RateCache<K, V> rateCache) {
        this.setRateCache(rateCache);
        return this;
    }

    public RateCache<K, V> getRateCache() {
        return rateCache;
    }

    public void setRateCache(RateCache<K, V> rateCache) {
        this.rateCache = rateCache;
    }

    public RateLimiterConfig<K, V> rateFactory(RateFactory rateFactory) {
        this.setRateFactory(rateFactory);
        return this;
    }

    public RateFactory getRateFactory() {
        return rateFactory;
    }

    public void setRateFactory(RateFactory rateFactory) {
        this.rateFactory = rateFactory;
    }

    public RateLimiterConfig<K, V> rateExceededListener(
            RateRecordedListener rateRecordedListener) {
        this.setRateExceededListener(rateRecordedListener);
        return this;
    }

    public RateRecordedListener getRateExceededListener() {
        return rateRecordedListener;
    }

    public void setRateExceededListener(RateRecordedListener rateRecordedListener) {
        this.rateRecordedListener = rateRecordedListener;
    }
}
