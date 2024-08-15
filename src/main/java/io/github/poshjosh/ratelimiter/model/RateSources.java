package io.github.poshjosh.ratelimiter.model;

public interface RateSources {

    RateSource NONE = RateSources.of("");

    static RateSource of(String id) {
        return of(Rates.ofId(id));
    }

    static RateSource of(Rates rates) {
        return new SimpleRateSource(rates);
    }
}
