package io.github.poshjosh.ratelimiter.performance;

import io.github.poshjosh.ratelimiter.RateLimiter;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidth;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;

class RateLimiterPerformanceIT {

    private static final long PERMITS_PER_SECOND = 1;

    // @TODO introduce maven profiles so performance tests could be INFO, while other tests DEBUG

    // Log level will affect the stats, so switch off logging

    @Test
    void tryAcquire_givenAllOrNothingBandwidth_shouldConsumeLimitedTimeAndMemory() {
        tryAcquire_givenBandwidth_shouldConsumeLimitedTimeAndMemory(Bandwidth.allOrNothing(PERMITS_PER_SECOND));
    }

    @Test
    void tryAcquire_givenBurstyBandwidth_shouldConsumeLimitedTimeAndMemory() {
        tryAcquire_givenBandwidth_shouldConsumeLimitedTimeAndMemory(Bandwidth.bursty(PERMITS_PER_SECOND));
    }

    @Test
    void tryAcquire_givenWarmingUpBandwidth_shouldConsumeLimitedTimeAndMemory() {
        tryAcquire_givenBandwidth_shouldConsumeLimitedTimeAndMemory(Bandwidth.warmingUp(PERMITS_PER_SECOND));
    }

    void tryAcquire_givenBandwidth_shouldConsumeLimitedTimeAndMemory(Bandwidth bandwidth) {
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(
                bandwidth, 100_000, Usage.of(20, 3_000_000));
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
        return RateLimiter.of(bandwidth);
    }
}