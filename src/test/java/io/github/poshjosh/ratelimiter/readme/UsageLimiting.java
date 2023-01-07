package io.github.poshjosh.ratelimiter.readme;

import io.github.poshjosh.ratelimiter.ResourceLimiter;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidth;

public class UsageLimiting {

    public static void main(String... args) {

        // 1 permit is allowed every second (for each unique resourceId)
        ResourceLimiter<String> resourceLimiter = ResourceLimiter.of(Bandwidth.allOrNothing(1));

        // These will return true
        resourceLimiter.tryConsume("resource_1");
        resourceLimiter.tryConsume("resource_2");

        // This will return false, it is the second consumption of resource_1
        resourceLimiter.tryConsume("resource_1");
    }
}
