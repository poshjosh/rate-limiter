package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.List;

public class RateLimiterSingletonTest extends RateLimiterTest {

    @Override
    public RateLimiter getRateLimiter(List<Rate> limits) {
        return new RateLimiterSingleton(getBaseRateSupplier(), limits);
    }
}
