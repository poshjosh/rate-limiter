package com.looseboxes.ratelimiter.annotation;

public interface AnnotatedElementIdProvider<SOURCE, ID> {
    ID getId(SOURCE source);
}
