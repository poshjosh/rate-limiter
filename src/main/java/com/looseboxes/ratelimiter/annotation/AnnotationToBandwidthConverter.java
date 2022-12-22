package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.BandwidthFactory;
import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.util.Operator;

final class AnnotationToBandwidthConverter implements AnnotationProcessor.Converter<Bandwidths> {

    AnnotationToBandwidthConverter() {}

    public boolean isOperatorEqual(Bandwidths bandwidths, Operator operator) {
        return bandwidths.getOperator().equals(operator);
    }

    @Override
    public Bandwidths convert(RateLimitGroup rateLimitGroup, RateLimit [] rateLimits) {
        final Operator operator = operator(rateLimitGroup);
        if (rateLimits.length == 0) {
            return Bandwidths.empty(operator);
        }
        Bandwidth[] bandwidths = new Bandwidth[rateLimits.length];
        for (int i = 0; i < rateLimits.length; i++) {
            bandwidths[i] = createBandwidth(rateLimits[i]);
        }
        return Bandwidths.of(operator, bandwidths);
    }

    private Operator operator(RateLimitGroup rateLimitGroup) {
        return rateLimitGroup == null ? AnnotationProcessor.DEFAULT_OPERATOR : rateLimitGroup.logic();
    }

    private Bandwidth createBandwidth(RateLimit rateLimit) {
        BandwidthFactory bandwidthFactory = BandwidthFactory.getOrCreateBandwidthFactory(rateLimit.factoryClass());
        return bandwidthFactory.createNew(rateLimit.limit(), rateLimit.duration(), rateLimit.timeUnit());
    }
}
