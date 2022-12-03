package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

public class DefaultRateFactory implements RateFactory {

    @Override
    public Rate createNew(long amount) {
        return Rate.of(amount, 0);
    }
}
