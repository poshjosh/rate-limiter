package com.looseboxes.ratelimiter.annotation.builder;

import com.looseboxes.ratelimiter.annotation.AnnotatedElementIdProvider;
import com.looseboxes.ratelimiter.util.RateFactory;
import com.looseboxes.ratelimiter.annotation.MethodLevelAnnotationRateFactory;

import java.lang.reflect.Method;
import java.util.List;

public class AnnotatedMethodRateLimiterBuilder<K> extends AbstractAnnotatedElementRateLimiterBuilder<Method, K> {

    @Override
    protected RateFactory<K> rateFactory(List<Class<?>> targetClasses, AnnotatedElementIdProvider<Method, K> annotatedElementIdProvider) {
        return new MethodLevelAnnotationRateFactory<>(targetClasses, annotatedElementIdProvider);
    }
}
