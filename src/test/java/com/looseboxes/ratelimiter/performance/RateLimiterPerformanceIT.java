package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.SingletonRateCache;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;
import org.junit.jupiter.api.Test;

import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class RateLimiterPerformanceIT {

    @Test
    public void testPerformance() {

        // @TODO introduce maven profiles so performance tests could be INFO, while other tests DEBUG

        // These stats were achieved under log level INFO

        testPerformance(1_000_000, 250, 50_000_000); // 2 Nov 2021
    }

    private void testPerformance(int count, long maxTime, long maxMemory) {
        RateLimiter<Integer> rateLimiter = getRateLimiterWithSingletonCache(count + 1, 60_000);
        final long tb4 = System.currentTimeMillis();
        final long mb4 = Util.availableMemory();
        final Integer key = Integer.valueOf(count);
        for(int i = 0; i < count; i++) {
            rateLimiter.record(key);
        }
        final long timeSpent = (System.currentTimeMillis() - tb4);
        final long memorySpent = Util.usedMemory(mb4);

        System.out.printf("Count: %s, Spent -> time: %d millis, memory: %d.%d MB or %d bytes", count, timeSpent,
                (memorySpent/1000000), (memorySpent%1000000), memorySpent);
        assertThat(timeSpent).isLessThan(maxTime);
        assertThat(memorySpent).isLessThanOrEqualTo(maxMemory);
    }

    public RateLimiter<Integer> getRateLimiter(int limit, int duration) {
        return new DefaultRateLimiter<>(new RateConfig().limit(limit).duration(duration).timeUnit(TimeUnit.MILLISECONDS));
    }

    public RateLimiter<Integer> getRateLimiterWithSingletonCache(int limit, int duration) {
        RateConfig rateConfig = new RateConfig().limit(limit).duration(duration).timeUnit(TimeUnit.MILLISECONDS);
        RateLimiterConfiguration rateLimiterConfiguration = new RateLimiterConfiguration<>()
                .rateCache(new SingletonRateCache<>(null))
                .rateRecordedListener(new RateExceededExceptionThrower())
                .rateFactory(new LimitWithinDurationFactory())
                .rateLimitConfig(new RateLimitConfig().addLimit(rateConfig));
        return new DefaultRateLimiter<Integer>(rateLimiterConfiguration);
    }
}