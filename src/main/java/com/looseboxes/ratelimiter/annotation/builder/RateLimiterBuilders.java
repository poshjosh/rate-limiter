package com.looseboxes.ratelimiter.annotation.builder;

import java.lang.reflect.Method;

public final class RateLimiterBuilders {

    public static <K> RateLimiterForAnnotatedElementBuilder<Class<?>, K> forAnnotatedClasses(Class<K> type) {
        return new RateLimiterForAnnotatedClassBuilder<K>();
    }

    public static <K> RateLimiterForAnnotatedElementBuilder<Method, K> forAnnotatedMethods(Class<K> type) {
        return new RateLimiterForAnnotatedMethodBuilder<K>();
    }
}
