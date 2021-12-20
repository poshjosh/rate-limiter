package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.SingletonRateCache;
import com.looseboxes.ratelimiter.util.RateConfig;
import com.looseboxes.ratelimiter.util.RateLimitConfig;
import org.junit.jupiter.api.Test;

import java.util.concurrent.TimeUnit;

public class RateLimiterPerformanceIT extends AbstractPerformanceTest{

    @Test
    public void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory() {

        // @TODO introduce maven profiles so performance tests could be INFO, while other tests DEBUG

        // These stats were achieved under log level INFO

        // 2 Nov 2021
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(1_000_000, 250, 50_000_000);
    }

    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(int count, long maxTime, long maxMemory) {

        RateLimiter<Integer> rateLimiter = getRateLimiterWithSingletonCache(count + 1, 60_000);

        recordCurrentTimeAndMemory();

        final Integer key = Integer.valueOf(count);
        for(int i = 0; i < count; i++) {
            rateLimiter.increment(key);
        }
        assertTimeSinceLastRecordIsLessThan(maxTime);
        assertMemorySinceLastRecordIsLessThan(maxMemory);
    }

    public RateLimiter<Integer> getRateLimiter(int limit, int duration) {
        return new SimpleRateLimiter<>(new RateConfig().limit(limit).duration(duration).timeUnit(TimeUnit.MILLISECONDS));
    }

    public RateLimiter<Integer> getRateLimiterWithSingletonCache(int limit, int duration) {
        RateConfig rateConfig = new RateConfig().limit(limit).duration(duration).timeUnit(TimeUnit.MILLISECONDS);
        RateLimiterConfiguration<Integer> rateLimiterConfiguration = new RateLimiterConfiguration<Integer>()
                .rateCache(new SingletonRateCache<>(null))
                .rateExceededListener(new RateExceededExceptionThrower())
                .rateFactory(new LimitWithinDurationFactory());
        return new SimpleRateLimiter<Integer>(rateLimiterConfiguration, new RateLimitConfig().addLimit(rateConfig));
    }
}