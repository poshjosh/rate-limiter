package com.looseboxes.ratelimiter;

final class DefaultRateFactory implements RateFactory {
    @Override
    public Rate createNew(long amount, long durationMillis) {
        return Rate.of(amount, durationMillis);
    }
}
