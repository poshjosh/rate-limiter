package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.Util;
import com.looseboxes.ratelimiter.annotations.RateLimit;
import com.looseboxes.ratelimiter.annotations.RateLimitGroup;
import com.looseboxes.ratelimiter.util.Operator;
import com.looseboxes.ratelimiter.util.Rate;
import com.looseboxes.ratelimiter.util.Rates;

import java.time.Duration;

final class AnnotationToRatesConverter implements AnnotationProcessor.Converter<Rates> {

    AnnotationToRatesConverter() {}

    public boolean isOperatorEqual(Rates rates, Operator operator) {
        return rates.getOperator().equals(operator);
    }

    @Override
    public Rates convert(RateLimitGroup rateLimitGroup, RateLimit[] rateLimits) {
        final Operator operator = operator(rateLimitGroup);
        if (rateLimits.length == 0) {
            return Rates.of(operator);
        }
        final Rate[] configs = new Rate[rateLimits.length];
        for (int i = 0; i < rateLimits.length; i++) {
            configs[i] = createRate(rateLimits[i]);
        }
        return Rates.of(operator, configs);
    }

    private Operator operator(RateLimitGroup rateLimitGroup) {
        return rateLimitGroup == null ? AnnotationProcessor.DEFAULT_OPERATOR : rateLimitGroup.operator();
    }

    private Rate createRate(RateLimit rateLimit) {
        Duration duration = Duration.of(rateLimit.duration(), Util.toChronoUnit(rateLimit.timeUnit()));
        //System.out.printf("%s AnnotationToRatesConverter factory: %s\n", java.time.LocalTime.now(), rateLimit.factoryClass());
        return Rate.of(rateLimit.limit(), duration, rateLimit.factoryClass());
    }
}
