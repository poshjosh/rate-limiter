package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.SingletonRateCache;
import com.looseboxes.ratelimiter.rates.AmountPerDuration;
import org.junit.jupiter.api.Test;

public class RateLimiterPerformanceIT extends AbstractPerformanceTest{

    @Test
    public void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory() {

        // @TODO introduce maven profiles so performance tests could be INFO, while other tests DEBUG

        // These stats were achieved under log level INFO

        // 2 Nov 2021
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(1_000_000, Usage.of(250, 50_000_000));
    }

    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(int count, Usage limit) {

        RateLimiter<Integer> rateLimiter = getRateLimiterWithSingletonCache(count + 1, 60_000);

        Usage bookmark = Usage.bookmark();

        final Integer key = Integer.valueOf(count);
        for(int i = 0; i < count; i++) {
            rateLimiter.consume(key);
        }

        assertUsageSinceBookmarkIsLessThan(bookmark, limit);
    }

    public RateLimiter<Integer> getRateLimiter(int limit, int duration) {
        return new SimpleRateLimiter<>(AmountPerDuration.of(limit, duration));
    }

    public RateLimiter<Integer> getRateLimiterWithSingletonCache(int limit, int duration) {
        return new SimpleRateLimiter<Integer>(AmountPerDuration.of(limit, duration)).withRateCache(new SingletonRateCache<>(null));
    }
}