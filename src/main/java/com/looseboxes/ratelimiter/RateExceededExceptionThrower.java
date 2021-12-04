package com.looseboxes.ratelimiter;

public class RateExceededExceptionThrower implements RateExceededListener {

    @Override
    public void onRateExceeded(RateExceededEvent rateExceededEvent) {
        throw new RateLimitExceededException(
                String.format("For: %s, rate: %s exceeds limit: %s",
                        rateExceededEvent.getKey(),
                        rateExceededEvent.getRate(),
                        rateExceededEvent.getExceededLimit()));
    }
}
