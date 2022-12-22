package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.util.Rate;

import java.time.Duration;

public class Concept {

    public static void main(String... args) {

        // 1 permit is allowed every 10 seconds (for each unique recording key)
        RateLimiter<String> rateLimiter = RateLimiter.of(Rate.of(1, Duration.ofSeconds(10)));

        // We use numbers as recording keys
        rateLimiter.tryConsume("resource_1");
        rateLimiter.tryConsume("resource_2");
        rateLimiter.tryConsume("resource_3");

        // This will return false, it is the second consumption of resource_1
        final boolean withinLimit = rateLimiter.tryConsume("resource_1");
        System.out.printf("Within limit: %b", withinLimit);
    }
}
