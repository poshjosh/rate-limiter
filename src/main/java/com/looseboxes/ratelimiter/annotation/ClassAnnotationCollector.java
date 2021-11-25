package com.looseboxes.ratelimiter.annotation;

public final class ClassAnnotationCollector extends AnnotationCollectorImpl<Class<?>> {
    public ClassAnnotationCollector() {
        super(new ClassNameProvider());
    }
}
