package com.looseboxes.ratelimiter.annotation;

public final class ClassAnnotationCollector extends DefaultAnnotationCollector<Class<?>>{
    public ClassAnnotationCollector() {
        super(Class::getName);
    }
}
