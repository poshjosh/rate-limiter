package com.looseboxes.ratelimiter.annotation;

import java.lang.annotation.*;
import java.util.concurrent.TimeUnit;

@Repeatable(RateLimit.List.class)
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface RateLimit {

    int limit() default Integer.MAX_VALUE;

    long duration() default 0;

    TimeUnit timeUnit() default TimeUnit.MILLISECONDS;

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE, ElementType.METHOD})
    @interface List {
        RateLimit[] value();
    }
}
