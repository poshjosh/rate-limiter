package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.util.SleepingTicker;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

final class RateLimiterProvider<K> {

    private final SleepingTicker ticker;
    private final Map<K, RateLimiter> resourceIdToRateLimiters;

    RateLimiterProvider(SleepingTicker ticker) {
        this.ticker = Objects.requireNonNull(ticker);
        this.resourceIdToRateLimiters = new ConcurrentHashMap<>();
    }

    Bandwidths provideBandwidths(K key, Bandwidths bandwidths) {
        return bandwidths.with(ticker.elapsedMicros());
    }

    RateLimiter provideRateLimiter(K key, Bandwidths bandwidths) {
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
