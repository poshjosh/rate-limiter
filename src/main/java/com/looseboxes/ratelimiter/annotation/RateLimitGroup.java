package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.rates.Rates;

import java.lang.annotation.*;

@Repeatable(RateLimitGroup.List.class)
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface RateLimitGroup {

    String name() default "";
    String value() default "";
    Rates.Logic logic() default Rates.Logic.OR;

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE, ElementType.METHOD})
    @interface List {
        RateLimitGroup[] value();
    }
}
