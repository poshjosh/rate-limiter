package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;

public class AmountPerDurationFactory implements RateFactory {

    @Override
    public Rate createNew() {
        return new AmountPerDuration();
    }
}
