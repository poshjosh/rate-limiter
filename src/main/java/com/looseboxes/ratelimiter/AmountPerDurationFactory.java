package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;

public class AmountPerDurationFactory implements RateFactory {

    private static final AmountPerDuration DEFAULT = AmountPerDuration.of(1, 0);

    @Override
    public Rate createNew() {
        return DEFAULT;
    }
}
