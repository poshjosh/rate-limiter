package io.github.poshjosh.ratelimiter.performance;

import io.github.poshjosh.ratelimiter.RateLimiter;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidth;
import org.junit.jupiter.api.Test;

class ResourceLimiterPerformanceIT {

    @Test
    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory() {

        // @TODO introduce maven profiles so performance tests could be INFO, while other tests DEBUG

        // Log level will affect the stats, so switch off logging

        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(1_000, 20_000, Usage.of(250, 50_000_000));
    }

    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(int count, int permitsPerSecond, Usage limit) {

        RateLimiter rateLimiter = getRateLimiter(permitsPerSecond);

        Usage bookmark = Usage.bookmark();

        for(int i = 0; i < count; i++) {
            rateLimiter.tryAcquire();
        }

        bookmark.assertUsageLessThan(limit);
    }

    public RateLimiter getRateLimiter(long permitsPerSecond) {
        return RateLimiter.of(Bandwidth.bursty(permitsPerSecond));
    }
}