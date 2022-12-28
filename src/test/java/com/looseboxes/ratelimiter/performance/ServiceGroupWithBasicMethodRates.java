package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.annotations.RateLimit;
import com.looseboxes.ratelimiter.annotations.RateLimitGroup;
import com.looseboxes.ratelimiter.util.Operator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.TimeUnit;

public class ServiceGroupWithBasicMethodRates {

    private static final Logger log = LoggerFactory.getLogger(ServiceGroupWithBasicMethodRates.class);

    public static class Service1{
        @RateLimit(limit = 2, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_2_per_minute() {
            log.info("limit_2_per_minute");
        }

        @RateLimit(limit = 5, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_5_per_minute() {
            log.info("limit_5_per_minute");
        }

        @RateLimit(limit = 2, duration = 1, timeUnit = TimeUnit.MINUTES)
        @RateLimit(limit = 5, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_2_or_5_per_minute() {
            log.info("limit_2_or_5_per_minute");
        }

        @RateLimitGroup(operator = Operator.AND)
        @RateLimit(limit = 2, duration = 1, timeUnit = TimeUnit.MINUTES)
        @RateLimit(limit = 5, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_2_and_5_per_minute() {
            log.info("limit_2_and_5_per_minute");
        }
    }

    public static class Service2{
        @RateLimit(limit = 2, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_2_per_minute() {
            log.info("limit_2_per_minute");
        }

        @RateLimit(limit = 5, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_5_per_minute() {
            log.info("limit_5_per_minute");
        }

        @RateLimit(limit = 2, duration = 1, timeUnit = TimeUnit.MINUTES)
        @RateLimit(limit = 5, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_2_or_5_per_minute() {
            log.info("limit_2_or_5_per_minute");
        }

        @RateLimitGroup(operator = Operator.AND)
        @RateLimit(limit = 2, duration = 1, timeUnit = TimeUnit.MINUTES)
        @RateLimit(limit = 5, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_2_and_5_per_minute() {
            log.info("limit_2_and_5_per_minute");
        }
    }

    public static class Service3{
        @RateLimit(limit = 2, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_2_per_minute() {
            log.info("limit_2_per_minute");
        }

        @RateLimit(limit = 5, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_5_per_minute() {
            log.info("limit_5_per_minute");
        }

        @RateLimit(limit = 2, duration = 1, timeUnit = TimeUnit.MINUTES)
        @RateLimit(limit = 5, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_2_or_5_per_minute() {
            log.info("limit_2_or_5_per_minute");
        }

        @RateLimitGroup(operator = Operator.AND)
        @RateLimit(limit = 2, duration = 1, timeUnit = TimeUnit.MINUTES)
        @RateLimit(limit = 5, duration = 1, timeUnit = TimeUnit.MINUTES)
        void limit_2_and_5_per_minute() {
            log.info("limit_2_and_5_per_minute");
        }
    }
}
