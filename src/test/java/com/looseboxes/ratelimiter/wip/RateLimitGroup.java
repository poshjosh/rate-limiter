package com.looseboxes.ratelimiter.wip;

import com.looseboxes.ratelimiter.rates.Rates;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface RateLimitGroup {
    String name() default "default";
    String value() default "default";
    Rates.Logic logic() default Rates.Logic.OR;
}
