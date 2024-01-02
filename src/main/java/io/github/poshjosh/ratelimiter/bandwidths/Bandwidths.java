package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.util.Operator;

public interface Bandwidths {

    Operator DEFAULT_OPERATOR = Operator.OR;

    static Bandwidth and(Bandwidth... bandwidths) {
        return of(Operator.AND, bandwidths);
    }

    static Bandwidth or(Bandwidth... bandwidths) {
        return of(Operator.OR, bandwidths);
    }

    static Bandwidth of(Bandwidth... bandwidths) {
        return of(Bandwidths.DEFAULT_OPERATOR, bandwidths);
    }

    static Bandwidth of(BandwidthArray bandwidths) {
        return new BandwidthArray(bandwidths);
    }

    static Bandwidth of(Operator operator, Bandwidth... bandwidths) {
        return of(BandwidthArray.buildId(operator, bandwidths), operator, bandwidths);
    }

    static Bandwidth of(String id, Operator operator, Bandwidth... bandwidths) {
        return new BandwidthArray(id, operator, bandwidths);
    }
}
