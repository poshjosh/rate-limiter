package com.wip.ratelimiter;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import com.wip.ratelimiter.rate.SmoothRate;

public class SmoothBurstyRateLimiter<K> implements RateLimiter<K> {

    private final RateLimiterIx delegate;

    public SmoothBurstyRateLimiter(Rate rate) {
        this(1.0, rate);
    }

    public SmoothBurstyRateLimiter(double maxBurstSeconds, Rate rate) {
        this.delegate = createDelegate(maxBurstSeconds, rate);
    }

    public RateLimiterIx createDelegate(double maxBurstSeconds, Rate rate) {
        double permitsPerSecond = (double)((AmountPerDuration)rate).getAmount() / 1000;
        RateLimiter2.SleepingStopwatch stopwatch = RateLimiter2.SleepingStopwatch.createFromSystemTimer();
        com.wip.ratelimiter.rate.Rate r = SmoothRate.bursty(permitsPerSecond, stopwatch.readMicros(), maxBurstSeconds);
        return new RateLimiter2(r, stopwatch);
    }

    @Override
    public boolean consume(Object context, K resourceId, int amount) {
        return this.delegate.tryAcquire(amount);
    }
}
