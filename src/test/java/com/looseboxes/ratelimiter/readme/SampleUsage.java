package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.annotation.RateLimiterFromAnnotationFactory;
import com.looseboxes.ratelimiter.annotations.RateLimit;

import java.util.concurrent.TimeUnit;

public class SampleUsage {

    static final int LIMIT = 3;

    static class RateLimitedResource {

        final RateLimiter rateLimiter;

        RateLimitedResource(RateLimiter rateLimiter) {
            this.rateLimiter = rateLimiter;
        }

        // Limited to 3 invocations every second
        @RateLimit(limit = LIMIT, duration = 1, timeUnit = TimeUnit.SECONDS)
        void rateLimitedMethod() {

            if (!rateLimiter.tryConsume("rateLimitedMethodId")) {
                throw new RuntimeException("Limit exceeded");
            }
        }
    }

    public static void main(String... args) {

        RateLimiter rateLimiter = RateLimiterFromAnnotationFactory.of().create(RateLimitedResource.class);

        RateLimitedResource rateLimitedResource = new RateLimitedResource(rateLimiter);

        int i = 0;
        for(; i < LIMIT; i++) {

            System.out.println("Invocation " + i + " of " + LIMIT);
            rateLimitedResource.rateLimitedMethod();
        }

        System.out.println("Invocation " + i + " of " + LIMIT + " should fail");
        // Should fail
        rateLimitedResource.rateLimitedMethod();
    }
}
