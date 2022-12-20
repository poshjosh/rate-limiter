package com.looseboxes.ratelimiter;

@FunctionalInterface
public interface RateFactory {

    static RateFactory newInstance() {
        return new DefaultRateFactory();
    }

    default Rate createNew(long amount) {
        return createNew(amount, 0);
    }

    Rate createNew(long amount, long durationMillis);
}
