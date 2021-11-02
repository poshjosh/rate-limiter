package com.looseboxes.ratelimiter.annotation.builder;

import com.looseboxes.ratelimiter.util.RateFactory;
import com.looseboxes.ratelimiter.annotation.AnnotatedElementIdProvider;
import com.looseboxes.ratelimiter.annotation.RateFactoryForMethodLevelAnnotation;

import java.lang.reflect.Method;
import java.util.List;

public class RateLimiterForAnnotatedMethodBuilder<K> extends AbstractRateLimiterForAnnotatedElementBuilder<Method, K> {

    @Override
    protected RateFactory<K> rateFactory(List<Class<?>> targetClasses, AnnotatedElementIdProvider<Method, K> annotatedElementIdProvider) {
        return new RateFactoryForMethodLevelAnnotation<>(targetClasses, annotatedElementIdProvider);
    }
}
