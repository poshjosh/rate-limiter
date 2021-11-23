package com.looseboxes.ratelimiter.annotation.builder;

import com.looseboxes.ratelimiter.util.RateFactory;
import com.looseboxes.ratelimiter.annotation.AnnotatedElementIdProvider;
import com.looseboxes.ratelimiter.annotation.ClassLevelAnnotationRateFactory;

import java.util.List;

public class AnnotatedClassRateLimiterBuilder<K> extends AbstractAnnotatedElementRateLimiterBuilder<Class<?>, K> {

    @Override
    protected RateFactory<K> rateFactory(List<Class<?>> targetClasses, AnnotatedElementIdProvider<Class<?>, K> annotatedElementIdProvider) {
        return new ClassLevelAnnotationRateFactory<>(targetClasses, annotatedElementIdProvider);
    }
}
