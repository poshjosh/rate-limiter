package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.cache.MapRateCache;
import com.looseboxes.ratelimiter.cache.RateCache;

import java.util.Objects;

public final class DefaultRateLimiterConfig<K, V> implements RateLimiterConfig<K, V> {

    private RateCache<K, V> rateCache;
    private RateFactory rateFactory;
    private RateRecordedListener rateRecordedListener;

    public DefaultRateLimiterConfig() {
        this(new MapRateCache<>(), new LimitWithinDurationFactory(), new RateExceededExceptionThrower());
    }

    public DefaultRateLimiterConfig(RateLimiterConfig<K, V> rateLimiterConfig) {
        this(rateLimiterConfig.getRateCache(), rateLimiterConfig.getRateFactory(), rateLimiterConfig.getRateRecordedListener());
    }

    public DefaultRateLimiterConfig(RateCache<K, V> rateCache, RateFactory rateFactory, RateRecordedListener rateRecordedListener) {
        this.rateCache = Objects.requireNonNull(rateCache);
        this.rateFactory = Objects.requireNonNull(rateFactory);
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
    }

    public DefaultRateLimiterConfig<K, V> rateCache(RateCache<K, V> rateCache) {
        this.setRateCache(rateCache);
        return this;
    }

    public RateCache<K, V> getRateCache() {
        return rateCache;
    }

    public void setRateCache(RateCache<K, V> rateCache) {
        this.rateCache = rateCache;
    }

    public DefaultRateLimiterConfig<K, V> rateFactory(RateFactory rateFactory) {
        this.setRateFactory(rateFactory);
        return this;
    }

    public RateFactory getRateFactory() {
        return rateFactory;
    }

    public void setRateFactory(RateFactory rateFactory) {
        this.rateFactory = rateFactory;
    }

    public DefaultRateLimiterConfig<K, V> rateRecordedListener(RateRecordedListener rateRecordedListener) {
        this.setRateRecordedListener(rateRecordedListener);
        return this;
    }

    public RateRecordedListener getRateRecordedListener() {
        return rateRecordedListener;
    }

    public void setRateRecordedListener(RateRecordedListener rateRecordedListener) {
        this.rateRecordedListener = rateRecordedListener;
    }
}
