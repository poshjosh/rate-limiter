package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;

public class LimitWithinDurationSupplier implements RateSupplier{

    @Override
    public Rate getInitialRate() {
        return new LimitWithinDuration();
    }
}
