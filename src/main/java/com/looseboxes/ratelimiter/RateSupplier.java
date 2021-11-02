package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.Rate;

@FunctionalInterface
public interface RateSupplier {
    Rate getInitialRate();
}
