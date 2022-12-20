package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.Rate;

public class Concept {

    public static void main(String... args) {

        // Only one consumption is allowed within a minute (for each unique recording key)
        RateLimiter<String> rateLimiter = RateLimiter.of(Rate.of(1,  60 * 1000));

        // We use numbers as recording keys
        rateLimiter.tryConsume("resource_1");
        rateLimiter.tryConsume("resource_2");
        rateLimiter.tryConsume("resource_3");

        // This will return false, it is the second consumption of resource_1
        final boolean withinLimit = rateLimiter.tryConsume("resource_1");
        System.out.printf("Within limit: %b", withinLimit);
    }
}
