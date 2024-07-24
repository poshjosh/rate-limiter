package io.github.poshjosh.ratelimiter.performance;

import io.github.poshjosh.ratelimiter.RateLimiter;
import io.github.poshjosh.ratelimiter.RateLimiters;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidth;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;

class RateLimiterPerformanceIT {

    private static final long PERMITS_PER_SECOND = 1;

    // @TODO introduce maven profiles so performance tests could be INFO, while other tests DEBUG

    // Log level will affect the stats, so switch off logging

    @Test
    void tryAcquire_givenAllOrNothingBandwidth_shouldConsumeLimitedTimeAndMemory() {
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                Bandwidths.allOrNothing(PERMITS_PER_SECOND), 10_000, Usage.of(10, 3000));
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                Bandwidths.allOrNothing(PERMITS_PER_SECOND), 100_000, Usage.of(20, 3_000_000));
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                Bandwidths.allOrNothing(PERMITS_PER_SECOND), 1_000_000, Usage.of(100, 30_000_000));
    }

    @Test
    void tryAcquire_givenBurstyBandwidth_shouldConsumeLimitedTimeAndMemory() {
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                Bandwidths.bursty(PERMITS_PER_SECOND), 10_000, Usage.of(10, 3000));
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                Bandwidths.bursty(PERMITS_PER_SECOND), 100_000, Usage.of(20, 3000));
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                Bandwidths.bursty(PERMITS_PER_SECOND), 1_000_000, Usage.of(50, 3000));
    }

    @Test
    void tryAcquire_givenWarmingUpBandwidth_shouldConsumeLimitedTimeAndMemory() {
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                Bandwidths.warmingUp(PERMITS_PER_SECOND), 10_000, Usage.of(10, 3000));
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                Bandwidths.warmingUp(PERMITS_PER_SECOND), 100_000, Usage.of(20, 3000));
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                Bandwidths.warmingUp(PERMITS_PER_SECOND), 1_000_000, Usage.of(50, 3000));
    }

    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
            Bandwidth bandwidth, int count, Usage usageLimit) {

        RateLimiter rateLimiter = getRateLimiter(bandwidth);

        Usage usageBookmark = Usage.bookmark();

        for(int i = 0; i < count; i++) {
            rateLimiter.tryAcquire();
        }

        Usage currentUsage = usageBookmark.current();
        final String bandwidthType = bandwidth.getClass().getSimpleName();
        System.out.println(bandwidthType + "\t  Total " + currentUsage + ", invocations: " + count);
        System.out.println(bandwidthType + "\tAverage " + currentUsage.divideBy(count));
        assertFalse(currentUsage.isAnyUsageGreaterThan(usageLimit),
                "Usage should be less or equal to limit, but was not.\nUsage: " +
                        currentUsage + "\nLimit: " + usageLimit);
    }

    public RateLimiter getRateLimiter(Bandwidth bandwidth) {
        return RateLimiters.of(bandwidth);
    }
}