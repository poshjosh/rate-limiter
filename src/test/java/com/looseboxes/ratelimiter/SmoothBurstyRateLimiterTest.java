package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;
import com.wip.ratelimiter.SmoothBurstyRateLimiter;

class SmoothBurstyRateLimiterTest extends AbstractRateLimiterTest{

    SmoothBurstyRateLimiterTest() {
        super(true);
    }

    public <T> RateLimiter<T> getRateLimiter(Rate... rates) {
        if (rates.length == 1) {
            return new SmoothBurstyRateLimiter<>(rates[0]);
        }
        throw new UnsupportedOperationException("Not supported");
    }

    @Override
    protected Rate [] getDefaultLimits() { return new Rate[]{getDefaultLimit()}; }

    @Override
    protected Rate [] getLimitsThatWillLeadToReset() {
        return new Rate [] {getBaseRate()};
    }
}
