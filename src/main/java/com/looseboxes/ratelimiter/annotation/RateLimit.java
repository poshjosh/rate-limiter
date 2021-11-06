package com.looseboxes.ratelimiter.annotation;

import java.lang.annotation.*;

@Repeatable(RateLimit.List.class)
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface RateLimit {

    int limit() default Integer.MAX_VALUE;

    long duration() default 0;

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE, ElementType.METHOD})
    @interface List {
        RateLimit[] value();
    }
}
