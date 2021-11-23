package com.looseboxes.ratelimiter.annotation.builder;

import java.lang.reflect.Method;

public final class RateLimiterBuilders {

    public static <K> AnnotatedElementRateLimiterBuilder<Class<?>, K> forAnnotatedClasses(Class<K> type) {
        return new AnnotatedClassRateLimiterBuilder<>();
    }

    public static <K> AnnotatedElementRateLimiterBuilder<Method, K> forAnnotatedMethods(Class<K> type) {
        return new AnnotatedMethodRateLimiterBuilder<>();
    }
}
