package com.looseboxes.ratelimiter.annotation.builder;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import com.looseboxes.ratelimiter.rates.Rates;
import com.looseboxes.ratelimiter.util.RateFactory;
import com.looseboxes.ratelimiter.annotation.AnnotatedElementIdProvider;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A builder for building rate limiters for elements like Class/Method
 * @param <SOURCE>
 * @param <ID>
 */
public abstract class AbstractRateLimiterForAnnotatedElementBuilder<SOURCE, ID>
        implements RateLimiterForAnnotatedElementBuilder<SOURCE, ID> {

    private List<Class<?>> targetClasses;
    private AnnotatedElementIdProvider<SOURCE, ID> annotatedElementIdProvider;
    private RateFactory<ID> rateFactory;
    private RateSupplier rateSupplier;
    private RateExceededHandler<ID> rateExceededHandler;

    protected abstract RateFactory<ID> rateFactory(List<Class<?>> targetClasses,
                                                     AnnotatedElementIdProvider<SOURCE, ID> annotatedElementIdProvider);

    public Map<ID, RateLimiter<ID>> build() {
        Map<ID, Rate> limits = rates();
        final Map<ID, RateLimiter<ID>> rateLimiters;
        if(limits.isEmpty()) {
            rateLimiters = Collections.emptyMap();
        }else{
            rateLimiters = new HashMap<>(limits.size(), 1.0f);
            limits.forEach((key, rate) -> {
                rateLimiters.put(key, new RateLimiterSingleton<>(
                        key, rateSupplier, Rates.Logic.OR, Collections.singletonList(rate), rateExceededHandler
                ));
            });
        }
        return rateLimiters.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(rateLimiters);
    }

    public Map<ID, Rate> rates() {
        if(rateSupplier == null) {
            rateSupplier = () -> new LimitWithinDuration();
        }
        if(rateExceededHandler == null) {
            rateExceededHandler = new RateExceededExceptionThrower<>();
        }
        if(rateFactory == null) {
            rateFactory = rateFactory(targetClasses, annotatedElementIdProvider);
        }
        return rateFactory.getRates();
    }

    public RateLimiterForAnnotatedElementBuilder<SOURCE, ID> targetClasses(List<Class<?>> targetClasses) {
        this.targetClasses = targetClasses;
        return this;
    }

    public RateLimiterForAnnotatedElementBuilder<SOURCE, ID> requestPathsProvider(AnnotatedElementIdProvider<SOURCE, ID> annotatedElementIdProvider) {
        this.annotatedElementIdProvider = annotatedElementIdProvider;
        return this;
    }

    public RateLimiterForAnnotatedElementBuilder<SOURCE, ID> rateFactory(RateFactory<ID> rateFactory) {
        this.rateFactory = rateFactory;
        return this;
    }

    public RateLimiterForAnnotatedElementBuilder<SOURCE, ID> rateSupplier(RateSupplier rateSupplier) {
        this.rateSupplier = rateSupplier;
        return this;
    }

    public RateLimiterForAnnotatedElementBuilder<SOURCE, ID> rateExceededHandler(RateExceededHandler<ID> rateExceededHandler) {
        this.rateExceededHandler = rateExceededHandler;
        return this;
    }
}
