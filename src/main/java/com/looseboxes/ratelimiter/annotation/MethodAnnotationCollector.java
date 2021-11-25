package com.looseboxes.ratelimiter.annotation;

import java.lang.reflect.Method;

public final class MethodAnnotationCollector extends AnnotationCollectorImpl<Method> {
    public MethodAnnotationCollector() {
        super(new MethodNameProvider());
    }
}
