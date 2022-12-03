package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

@FunctionalInterface
public interface RateFactory {
    Rate createNew(long amount);
}
