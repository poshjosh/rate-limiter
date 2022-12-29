package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.SleepingTicker;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

final class DefaultResourceLimiterConfig<K, V> implements ResourceLimiterConfig<K, V>,
        ResourceLimiterConfig.Builder<K, V> {

    static class DefaultRateLimiterProvider<K> implements RateLimiterProvider<K> {

        private final SleepingTicker ticker;
        private final Map<K, RateLimiter> resourceIdToRateLimiters;

        DefaultRateLimiterProvider(SleepingTicker ticker) {
            this.ticker = Objects.requireNonNull(ticker);
            this.resourceIdToRateLimiters = new ConcurrentHashMap<>();
        }

        @Override
        public Bandwidths initFrom(Bandwidths bandwidths) {
            return bandwidths.with(ticker.elapsedMicros());
        }

        @Override
        public RateLimiter provideRateLimiter(K key, Bandwidths bandwidths) {
            RateLimiter value;
            if ((value = this.resourceIdToRateLimiters.get(key)) == null) {
                RateLimiter newValue;
                if ((newValue = createNew(bandwidths)) != null) {
                    this.resourceIdToRateLimiters.put(key, newValue);
                    return newValue;
                }
            }
            return value;
        }

        private RateLimiter createNew(Bandwidths bandwidths) {
            return RateLimiter.of(bandwidths, ticker);
        }
    }

    private RateCache<K, V> cache;
    private ResourceUsageListener usageListener;
    private RateLimiterProvider<K> rateLimiterProvider;

    DefaultResourceLimiterConfig() {
        this(RateCache.ofMap(),
                ResourceUsageListener.NO_OP,
                new DefaultRateLimiterProvider<>(
                        SleepingTicker.zeroOffset()
                ));
    }

    DefaultResourceLimiterConfig(ResourceLimiterConfig<K, V> resourceLimiterConfig) {
        this(resourceLimiterConfig.getCache(),
                resourceLimiterConfig.getUsageListener(),
                resourceLimiterConfig.getRateLimiterProvider());
    }

    DefaultResourceLimiterConfig(
            RateCache<K, V> cache,
            ResourceUsageListener usageListener,
            RateLimiterProvider<K> rateLimiterProvider) {
        this.cache = Objects.requireNonNull(cache);
        this.usageListener = Objects.requireNonNull(usageListener);
        this.rateLimiterProvider = Objects.requireNonNull(rateLimiterProvider);
    }

    @Override
    public DefaultResourceLimiterConfig<K, V> build() {
        return new DefaultResourceLimiterConfig<>(this);
    }

    @Override
    public DefaultResourceLimiterConfig<K, V> cache(RateCache<K, V> rateCache) {
        this.setCache(rateCache);
        return this;
    }

    public RateCache<K, V> getCache() {
        return cache;
    }

    public void setCache(RateCache<K, V> cache) {
        this.cache = cache;
    }

    @Override
    public DefaultResourceLimiterConfig<K, V> usageListener(ResourceUsageListener resourceUsageListener) {
        this.setUsageListener(resourceUsageListener);
        return this;
    }

    @Override
    public ResourceUsageListener getUsageListener() {
        return usageListener;
    }

    public void setUsageListener(ResourceUsageListener resourceUsageListener) {
        this.usageListener = resourceUsageListener;
    }

    @Override
    public DefaultResourceLimiterConfig<K, V> rateLimiterProvider(
            RateLimiterProvider<K> rateLimiterProvider) {
        this.rateLimiterProvider = rateLimiterProvider;
        return this;
    }

    @Override
    public RateLimiterProvider<K> getRateLimiterProvider() {
        return rateLimiterProvider;
    }

    public void setRateLimiterProvider(RateLimiterProvider<K> rateLimiterProvider) {
        this.rateLimiterProvider = rateLimiterProvider;
    }
}
