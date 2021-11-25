package com.looseboxes.ratelimiter.annotation;

public interface AnnotationProcessor<S> {
    <R> R process(S source, AnnotationCollector<S, R> collector);
}
