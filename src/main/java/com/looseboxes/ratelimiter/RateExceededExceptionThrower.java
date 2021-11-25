package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

public class RateExceededExceptionThrower implements RateRecordedListener {

    @Override
    public void onRateRecorded(Object key, Rate rate) { }

    @Override
    public void onRateExceeded(Object key, Rate rate, Rate exceededRate) {
        throw new RateLimitExceededException(String.format("For: %s, rate: %s exceeds limit: %s", key, rate, exceededRate));
    }
}
