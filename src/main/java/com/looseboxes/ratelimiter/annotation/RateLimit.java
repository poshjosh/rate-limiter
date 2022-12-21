package com.looseboxes.ratelimiter.annotation;

import com.looseboxes.ratelimiter.BandwidthFactory;

import java.lang.annotation.*;
import java.util.concurrent.TimeUnit;

@Repeatable(RateLimit.List.class)
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface RateLimit {

    long limit() default Long.MAX_VALUE;

    long duration() default 0;

    TimeUnit timeUnit() default TimeUnit.MILLISECONDS;

    Class<? extends BandwidthFactory> factoryClass() default BandwidthFactory.SmoothBurstyBandwidthFactory.class;

    @Documented
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE, ElementType.METHOD})
    @interface List {
        RateLimit[] value();
    }
}
