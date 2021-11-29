package com.looseboxes.ratelimiter;

public class RateExceededExceptionThrower implements RateRecordedListener {

    @Override
    public void onRateRecorded(RateRecordedEvent rateRecordedEvent) {
        if(rateRecordedEvent.isLimitExceeded()) {
            throw new RateLimitExceededException(
                    String.format("For: %s, rate: %s exceeds limit: %s",
                    rateRecordedEvent.getKey(),
                    rateRecordedEvent.getRate(),
                    rateRecordedEvent.getExceededLimitOptional().orElse(null)));
        }
    }
}
