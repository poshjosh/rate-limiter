package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateExceededException;
import com.looseboxes.ratelimiter.SimpleRateLimiter;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import com.looseboxes.ratelimiter.rates.Rate;

public class Concept {

    public static void main(String... args) {

        // Only one recording is allowed within a minute (for each unique recording key)
        Rate rate = AmountPerDuration.of(1,  60 * 1000);

        RateLimiter<Integer> rateLimiter = new SimpleRateLimiter<>(rate);

        // We use numbers as recording keys
        rateLimiter.consume(1);
        rateLimiter.consume(2);
        rateLimiter.consume(3);

        // This will fail, it is the second recording of the number 1
        try {
            rateLimiter.consume(1);
        }catch(RateExceededException e) {
            e.printStackTrace();
        }
    }
}
