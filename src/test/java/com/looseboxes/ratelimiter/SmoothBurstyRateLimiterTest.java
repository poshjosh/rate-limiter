package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.CompositeRate;

class SmoothBurstyRateLimiterTest extends AbstractRateLimiterTest{

    SmoothBurstyRateLimiterTest() {
        super(false);
    }

    @Override
    public <T> RateLimiter<T> getRateLimiter(Rate... rates) {
        return RateLimiter.bursty(CompositeRate.of(rates));
    }
}