package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;

public class AmountPerDurationFactory implements RateFactory {

    @Override
    public Rate createNew() {
        // We could have shared a single instance, since the instances are immutable.
        // However, the time created is of importance. Therefore we return a new instance, each time.
        return AmountPerDuration.of(1, 0);
    }
}
