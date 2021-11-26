package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;

public class LimitWithinDurationFactory implements RateFactory {

    @Override
    public Rate createNew() {
        return new LimitWithinDuration();
    }
}
