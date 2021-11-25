package com.looseboxes.ratelimiter.annotation;

import java.lang.reflect.Method;

public final class MethodAnnotationProcessor implements AnnotationProcessor<Method>{

    public MethodAnnotationProcessor() { }

    @Override
    public <R> R process(Method method, AnnotationCollector<Method, R> collector) {
        final RateLimitGroup [] rateLimitGroups = method.getAnnotationsByType(RateLimitGroup.class);
        for(RateLimitGroup rateLimitGroup : rateLimitGroups) {
            collector.collect(method, rateLimitGroup);
        }
        final RateLimit [] rateLimits = method.getAnnotationsByType(RateLimit.class);
        for(RateLimit rateLimit : rateLimits) {
            collector.collect(method, rateLimit);
        }
        return collector.getResult();
    }
}
