package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateExceededException;
import com.looseboxes.ratelimiter.SimpleRateLimiter;
import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.util.RateConfig;

import java.util.concurrent.TimeUnit;

public class Concept {

    public static void main(String... args) {

        // Only one recording is allowed within a minute (for each unique recording key)
        RateConfig rateConfig = new RateConfig().limit(1).duration(1).timeUnit(TimeUnit.MINUTES);

        RateLimiter<Integer> rateLimiter = new SimpleRateLimiter<>(rateConfig);

        // We use numbers as recording keys
        rateLimiter.increment(1);
        rateLimiter.increment(2);
        rateLimiter.increment(3);

        // This will fail, it is the second recording of the number 1
        try {
            rateLimiter.increment(1);
        }catch(RateExceededException e) {
            System.err.println(e);
        }
    }
}
