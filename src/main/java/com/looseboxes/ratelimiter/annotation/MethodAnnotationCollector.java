package com.looseboxes.ratelimiter.annotation;

import java.lang.reflect.Method;

public final class MethodAnnotationCollector extends DefaultAnnotationCollector<Method>{
    public MethodAnnotationCollector() {
        super(method -> method.getDeclaringClass().getName() + '_' + method.getName());
    }
}
