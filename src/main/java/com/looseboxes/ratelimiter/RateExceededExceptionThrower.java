package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

import java.util.Collection;

public class RateExceededExceptionThrower implements RateRecordedListener {

    @Override
    public void onRateExceeded(Object source, Object key, int recordedHits, Collection<Rate> exceededLimits) {
        throw new RateExceededException(
                String.format("For: %s, Limits exceeded: %s", key, exceededLimits.isEmpty() ? false : exceededLimits));
    }
}
