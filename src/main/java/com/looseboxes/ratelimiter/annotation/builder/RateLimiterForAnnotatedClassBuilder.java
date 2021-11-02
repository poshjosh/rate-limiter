package com.looseboxes.ratelimiter.annotation.builder;

import com.looseboxes.ratelimiter.util.RateFactory;
import com.looseboxes.ratelimiter.annotation.AnnotatedElementIdProvider;
import com.looseboxes.ratelimiter.annotation.RateFactoryForClassLevelAnnotation;

import java.util.List;

public class RateLimiterForAnnotatedClassBuilder<K> extends AbstractRateLimiterForAnnotatedElementBuilder<Class<?>, K> {

    @Override
    protected RateFactory<K> rateFactory(List<Class<?>> targetClasses, AnnotatedElementIdProvider<Class<?>, K> annotatedElementIdProvider) {
        return new RateFactoryForClassLevelAnnotation<>(targetClasses, annotatedElementIdProvider);
    }
}
