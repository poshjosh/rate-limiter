package com.looseboxes.ratelimiter.annotation.builder;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.annotation.AnnotatedElementIdProvider;
import com.looseboxes.ratelimiter.annotation.RateComposition;
import com.looseboxes.ratelimiter.util.RateFactory;

import java.util.*;

/**
 * A builder for building rate limiters for elements like Class/Method
 * @param <SOURCE>
 * @param <ID>
 */
public abstract class AbstractAnnotatedElementRateLimiterBuilder<SOURCE, ID>
        implements AnnotatedElementRateLimiterBuilder<SOURCE, ID> {

    private List<Class<?>> targetClasses;
    private AnnotatedElementIdProvider<SOURCE, ID> annotatedElementIdProvider;
    private RateFactory<ID> rateFactory;
    private RateSupplier rateSupplier;
    private RateExceededHandler rateExceededHandler;

    protected abstract RateFactory<ID> rateFactory(List<Class<?>> targetClasses,
                                                     AnnotatedElementIdProvider<SOURCE, ID> annotatedElementIdProvider);

    public Map<ID, RateLimiter<ID>> build() {
        List<RateComposition<ID>> limits = rates();
        final Map<ID, RateLimiter<ID>> rateLimiters;
        if(limits.isEmpty()) {
            rateLimiters = Collections.emptyMap();
        }else{
            rateLimiters = new HashMap<>(limits.size(), 1.0f);
            for (RateComposition<ID> limit : limits) {
                rateLimiters.put(limit.getId(), new SingletonRateLimiter<>(
                        limit.getId(), rateSupplier, limit.getLogic(), rateExceededHandler, limit.getRates()
                ));
            }
        }
        return rateLimiters.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(rateLimiters);
    }

    public List<RateComposition<ID>> rates() {
        if(rateSupplier == null) {
            rateSupplier = new LimitWithinDurationSupplier();
        }
        if(rateExceededHandler == null) {
            rateExceededHandler = new RateExceededExceptionThrower();
        }
        if(rateFactory == null) {
            rateFactory = rateFactory(targetClasses, annotatedElementIdProvider);
        }
        return rateFactory.getRates();
    }

    public AnnotatedElementRateLimiterBuilder<SOURCE, ID> targetClasses(List<Class<?>> targetClasses) {
        this.targetClasses = targetClasses;
        return this;
    }

    public AnnotatedElementRateLimiterBuilder<SOURCE, ID> requestPathsProvider(AnnotatedElementIdProvider<SOURCE, ID> annotatedElementIdProvider) {
        this.annotatedElementIdProvider = annotatedElementIdProvider;
        return this;
    }

    public AnnotatedElementRateLimiterBuilder<SOURCE, ID> rateFactory(RateFactory<ID> rateFactory) {
        this.rateFactory = rateFactory;
        return this;
    }

    public AnnotatedElementRateLimiterBuilder<SOURCE, ID> rateSupplier(RateSupplier rateSupplier) {
        this.rateSupplier = rateSupplier;
        return this;
    }

    public AnnotatedElementRateLimiterBuilder<SOURCE, ID> rateExceededHandler(RateExceededHandler rateExceededHandler) {
        this.rateExceededHandler = rateExceededHandler;
        return this;
    }
}
