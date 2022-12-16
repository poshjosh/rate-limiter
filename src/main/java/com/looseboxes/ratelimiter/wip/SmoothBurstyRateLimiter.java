package com.looseboxes.ratelimiter.wip;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.wip.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.wip.bandwidths.SmoothBandwidth;

public class SmoothBurstyRateLimiter<K> implements RateLimiter<K> {

    private final BandwidthLimiter delegate;

    public SmoothBurstyRateLimiter(Rate rate) {
        AmountPerDuration amountPerDuration = (AmountPerDuration)rate;
        SimpleBandwidthLimiter.SleepingStopwatch stopwatch = SimpleBandwidthLimiter.SleepingStopwatch.createFromSystemTimer();
        Bandwidth bandwidth = SmoothBandwidth.bursty(amountPerDuration, stopwatch.readMicros());
        this.delegate = new SimpleBandwidthLimiter(bandwidth, stopwatch);
    }

    @Override
    public boolean consume(Object context, K resourceId, int amount) {
        return this.delegate.tryAcquire(amount);
    }

    public BandwidthLimiter getDelegate() {
        return delegate;
    }
}
