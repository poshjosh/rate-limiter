package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

public class RateExceededExceptionThrower implements RateExceededHandler{

    @Override
    public void onRateExceeded(Object key, Rate rate, Rate limit) {
        throw new RateLimitExceededException(String.format("For: %s, rate: %s exceeds limit: %s", key, rate, limit));
    }
}
