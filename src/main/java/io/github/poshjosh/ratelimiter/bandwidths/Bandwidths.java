package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Rate;
import io.github.poshjosh.ratelimiter.model.Rates;
import io.github.poshjosh.ratelimiter.util.Operator;

public interface Bandwidths {

    Operator DEFAULT_OPERATOR = Operator.OR;

    static Bandwidth ofSeconds(int permits) {
        return of(Rate.ofSeconds(permits));
    }

    static Bandwidth of(Rate rate) {
        return RateToBandwidthConverter.ofDefaults().convert(rate);
    }

    static Bandwidth of(Rates rates) {
        return RateToBandwidthConverter.ofDefaults().convert(rates);
    }

    static Bandwidth and(Bandwidth... bandwidths) {
        return of(Operator.AND, bandwidths);
    }

    static Bandwidth or(Bandwidth... bandwidths) {
        return of(Operator.OR, bandwidths);
    }

    static Bandwidth of(Bandwidth... bandwidths) {
        return of(Bandwidths.DEFAULT_OPERATOR, bandwidths);
    }

    static Bandwidth of(Operator operator, Bandwidth... bandwidths) {
        return of(BandwidthArray.buildId(operator, bandwidths), operator, bandwidths);
    }

    static Bandwidth of(String id, Operator operator, Bandwidth... bandwidths) {
        return BandwidthArray.of(id, operator, bandwidths);
    }
}
