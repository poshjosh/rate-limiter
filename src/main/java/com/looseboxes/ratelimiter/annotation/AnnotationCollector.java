package com.looseboxes.ratelimiter.annotation;

public interface AnnotationCollector<S, R> {
    AnnotationCollector<S, R> collect(S source, RateLimitGroup rateLimitGroup);
    AnnotationCollector<S, R> collect(S source, RateLimit rateLimit);
    R getResult();
}
