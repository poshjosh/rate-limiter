package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.SingletonRateCache;
import com.looseboxes.ratelimiter.util.RateConfig;
import org.junit.jupiter.api.Test;

import java.time.Duration;

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
            rateLimiter.increment(key);
        }

        assertUsageSinceBookmarkIsLessThan(bookmark, limit);
    }

    public RateLimiter<Integer> getRateLimiter(int limit, int duration) {
        return new SimpleRateLimiter<>(new RateConfig().limit(limit).duration(Duration.ofMillis(duration)));
    }

    public RateLimiter<Integer> getRateLimiterWithSingletonCache(int limit, int duration) {
        RateConfig rateConfig = new RateConfig().limit(limit).duration(Duration.ofMillis(duration));
        return new SimpleRateLimiter<Integer>(rateConfig).withRateCache(new SingletonRateCache<>(null));
    }
}