package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Logic;

import java.lang.annotation.*;

@Repeatable(RateLimitGroup.List.class)
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface RateLimitGroup {

    String name() default "";
    String value() default "";
    Logic logic() default Logic.OR;

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE, ElementType.METHOD})
    @interface List {
        RateLimitGroup[] value();
    }
}
