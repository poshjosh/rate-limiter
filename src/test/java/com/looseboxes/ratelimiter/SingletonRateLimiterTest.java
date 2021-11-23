package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.List;

public class SingletonRateLimiterTest extends RateLimiterTest {

    @Override
    public RateLimiter getRateLimiter(List<Rate> limits) {
        return new SingletonRateLimiter(limits.toArray(new Rate[0]));
    }
}
