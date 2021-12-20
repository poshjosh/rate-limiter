package com.looseboxes.ratelimiter;

public class RateExceededExceptionThrower implements RateExceededListener {

    @Override
    public void onRateExceeded(RateExceededEvent rateExceededEvent) {
        throw new RateLimitExceededException(
                String.format("For: %s, Limit exceeded: %s",
                        rateExceededEvent.getKey(),
                        rateExceededEvent.getExceededLimit()));
    }
}
