package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.rates.Rate;

public class Concept {

    public static void main(String... args) {

        // Only one recording is allowed within a minute (for each unique recording key)
        RateLimiter<Integer> rateLimiter = RateLimiter.of(Rate.of(1,  60 * 1000));

        // We use numbers as recording keys
        rateLimiter.consume(1);
        rateLimiter.consume(2);
        rateLimiter.consume(3);

        // This will return false, it is the second recording of the number 1
        final boolean withinLimit = rateLimiter.consume(1);
        System.out.printf("Within limit: %b", withinLimit);
    }
}
