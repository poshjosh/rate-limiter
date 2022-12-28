package com.looseboxes.ratelimiter;

class RateLimiterTest extends AbstractRateLimiterTest{
    RateLimiterTest() {
        super(BandwidthFactory.AllOrNothing.class, false);
    }
}