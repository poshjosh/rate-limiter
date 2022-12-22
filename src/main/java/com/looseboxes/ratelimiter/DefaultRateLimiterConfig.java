package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.SleepingTicker;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

final class DefaultRateLimiterConfig<K, V> implements RateLimiterConfig<K, V>,
        RateLimiterConfig.Builder<K, V> {

    static class DefaultBandwidthLimiterProvider<K> implements BandwidthLimiterProvider<K> {

        private final SleepingTicker ticker;
        private final Map<K, BandwidthLimiter> resourceIdToBandwidthLimiters;

        DefaultBandwidthLimiterProvider(SleepingTicker ticker) {
            this.ticker = Objects.requireNonNull(ticker);
            this.resourceIdToBandwidthLimiters = new ConcurrentHashMap<>();
        }

        @Override
        public SleepingTicker getTicker() {
            return ticker;
        }

        @Override
        public BandwidthLimiter getBandwidthLimiter(K key, Bandwidths bandwidths) {
            BandwidthLimiter value;
            if ((value = this.resourceIdToBandwidthLimiters.get(key)) == null) {
                BandwidthLimiter newValue;
                if ((newValue = createNew(bandwidths)) != null) {
                    this.resourceIdToBandwidthLimiters.put(key, newValue);
                    return newValue;
                }
            }
            return value;
        }

        private BandwidthLimiter createNew(Bandwidths bandwidths) {
            return new DefaultBandwidthLimiter(bandwidths, getTicker());
        }
    }

    private RateCache<K, V> rateCache;
    private RateRecordedListener rateRecordedListener;
    private BandwidthLimiterProvider<K> bandwidthLimiterProvider;

    DefaultRateLimiterConfig() {
        this(RateCache.ofMap(),
                RateRecordedListener.NO_OP,
                new DefaultBandwidthLimiterProvider<>(
                        SleepingTicker.zeroOffset()
                ));
    }

    DefaultRateLimiterConfig(RateLimiterConfig<K, V> rateLimiterConfig) {
        this(rateLimiterConfig.getRateCache(),
                rateLimiterConfig.getRateRecordedListener(),
                rateLimiterConfig.getBandwidthLimiterProvider());
    }

    DefaultRateLimiterConfig(
            RateCache<K, V> rateCache,
            RateRecordedListener rateRecordedListener,
            BandwidthLimiterProvider<K> bandwidthLimiterProvider) {
        this.rateCache = Objects.requireNonNull(rateCache);
        this.rateRecordedListener = Objects.requireNonNull(rateRecordedListener);
        this.bandwidthLimiterProvider = Objects.requireNonNull(bandwidthLimiterProvider);
    }

    @Override
    public DefaultRateLimiterConfig<K, V> build() {
        return new DefaultRateLimiterConfig<>(this);
    }

    @Override
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

    @Override
    public DefaultRateLimiterConfig<K, V> rateRecordedListener(RateRecordedListener rateRecordedListener) {
        this.setRateRecordedListener(rateRecordedListener);
        return this;
    }

    @Override
    public RateRecordedListener getRateRecordedListener() {
        return rateRecordedListener;
    }

    public void setRateRecordedListener(RateRecordedListener rateRecordedListener) {
        this.rateRecordedListener = rateRecordedListener;
    }

    @Override
    public DefaultRateLimiterConfig<K, V> bandwidthLimiterProvider(
            BandwidthLimiterProvider<K> bandwidthLimiterProvider) {
        this.bandwidthLimiterProvider = bandwidthLimiterProvider;
        return this;
    }

    @Override
    public BandwidthLimiterProvider<K> getBandwidthLimiterProvider() {
        return bandwidthLimiterProvider;
    }

    public void setBandwidthLimiterProvider(BandwidthLimiterProvider<K> bandwidthLimiterProvider) {
        this.bandwidthLimiterProvider = bandwidthLimiterProvider;
    }
}
