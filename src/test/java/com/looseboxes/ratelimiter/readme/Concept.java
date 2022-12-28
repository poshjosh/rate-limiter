package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.util.Rate;

import java.time.Duration;

public class Concept {

    public static void main(String... args) {

        // 1 permit is allowed every 10 seconds (for each unique recording key)
        RateLimiter<String> rateLimiter = RateLimiter.of(Rate.of(1, Duration.ofSeconds(1)));

        boolean withinLimit;

        // We use numbers as recording keys
        // These will return true
        withinLimit = rateLimiter.tryConsume("resource_1");
        withinLimit = rateLimiter.tryConsume("resource_2");
        withinLimit = rateLimiter.tryConsume("resource_3");
        System.out.printf("Within limit: %b\n", withinLimit);

        // This will return false, it is the second consumption of resource_1
        withinLimit = rateLimiter.tryConsume("resource_1");
        System.out.printf("Within limit: %b\n", withinLimit);
    }
}
