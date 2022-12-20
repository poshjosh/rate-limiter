package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.util.Operator;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface RateLimitGroup {

    String name() default "";
    String value() default "";
    Operator logic() default Operator.OR;
}
