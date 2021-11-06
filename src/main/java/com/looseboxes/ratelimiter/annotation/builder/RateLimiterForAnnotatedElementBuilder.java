package com.looseboxes.ratelimiter.annotation.builder;

import com.looseboxes.ratelimiter.RateExceededHandler;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.RateSupplier;
import com.looseboxes.ratelimiter.annotation.AnnotatedElementIdProvider;
import com.looseboxes.ratelimiter.annotation.RateComposition;
import com.looseboxes.ratelimiter.util.RateFactory;

import java.util.Collections;
import java.util.List;
import java.util.Map;

public interface RateLimiterForAnnotatedElementBuilder<SOURCE, ID> {

    Map<ID, RateLimiter<ID>> build();

    List<RateComposition<ID>> rates();

    default RateLimiterForAnnotatedElementBuilder<SOURCE, ID> targetClass(Class<?> targetClass) {
        return targetClasses(Collections.singletonList(targetClass));
    }

    RateLimiterForAnnotatedElementBuilder<SOURCE, ID> targetClasses(List<Class<?>> targetClasses);

    RateLimiterForAnnotatedElementBuilder<SOURCE, ID> requestPathsProvider(AnnotatedElementIdProvider<SOURCE, ID> annotatedElementIdProvider);

    RateLimiterForAnnotatedElementBuilder<SOURCE, ID> rateFactory(RateFactory<ID> rateFactory);

    RateLimiterForAnnotatedElementBuilder<SOURCE, ID> rateSupplier(RateSupplier rateSupplier);

    RateLimiterForAnnotatedElementBuilder<SOURCE, ID> rateExceededHandler(RateExceededHandler rateExceededHandler);
}
