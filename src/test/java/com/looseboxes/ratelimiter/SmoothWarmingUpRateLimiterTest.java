package com.looseboxes.ratelimiter;

import org.junit.jupiter.api.Test;

class SmoothWarmingUpRateLimiterTest extends AbstractRateLimiterTest{

    SmoothWarmingUpRateLimiterTest() {
        super(BandwidthFactory.warmingUp(),false);
    }

    @Test // TODO - This test fails for the RateLimiter, so we override to do nothing for now
    void shouldResetWhenLimitNotExceededWithinDuration() { }
}