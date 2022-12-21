package com.looseboxes.ratelimiter;

class SmoothBurstyRateLimiterTest extends AbstractRateLimiterTest{
    SmoothBurstyRateLimiterTest() {
        super(BandwidthFactory.bursty(), false);
    }
}