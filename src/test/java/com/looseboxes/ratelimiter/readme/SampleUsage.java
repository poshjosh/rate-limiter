package com.looseboxes.ratelimiter.readme;

import com.looseboxes.ratelimiter.RateLimiter;
import com.looseboxes.ratelimiter.annotation.RateLimit;
import com.looseboxes.ratelimiter.builder.RateLimitersBuilder;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class SampleUsage {

    static class RateLimitedResource {

        static final int LIMIT = 3;

        final RateLimiter<Object> rateLimiter;
        final String rateLimitedMethodId;

        RateLimitedResource(RateLimiter<Object> rateLimiter) {
            this.rateLimiter = Objects.requireNonNull(rateLimiter);
            this.rateLimitedMethodId = getClass().getName() + ".rateLimitedMethod";
        }

        // Limited to 3 invocations every 2 seconds OR 100 invocations every 1 minute
        @RateLimit(limit = LIMIT, duration = 2000)
        @RateLimit(limit = 100, duration = 1, timeUnit = TimeUnit.MINUTES)
        void rateLimitedMethod() {

            // VERY IMPORTANT to record usage
            rateLimiter.consume(rateLimitedMethodId);
        }
    }

    public static void main(String... args) {

        RateLimiter<Object> rateLimiter = buildRateLimiter(RateLimitedResource.class);

        RateLimitedResource rateLimitedResource = new RateLimitedResource(rateLimiter);

        // This will make the last invocation of the method from within the for loop fail
        final int exceedsLimitByOne = RateLimitedResource.LIMIT + 1;

        for(int i = 0; i < exceedsLimitByOne; i++) {

            rateLimitedResource.rateLimitedMethod();
        }
    }

    private static RateLimiter<Object> buildRateLimiter(Class<?> clazz) {
        return RateLimitersBuilder.list().build(clazz).get(0); // Only one class/method is rate limited
    }
}
