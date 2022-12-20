package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.CompositeRate;
import com.looseboxes.ratelimiter.Rate;
import org.junit.jupiter.api.Test;

class RateLimiterPerformanceIT extends AbstractPerformanceTest{

    @Test
    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory() {

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
            rateLimiter.tryConsume(key);
        }

        assertUsageSinceBookmarkIsLessThan(bookmark, limit);
    }

    public RateLimiter<Integer> getRateLimiter(int limit, int duration) {
        return RateLimiter.of(Rate.of(limit, duration));
    }

    public RateLimiter<Integer> getRateLimiterWithSingletonCache(int limit, int duration) {
    RateLimiterConfig<Integer, ?> config =
        RateLimiterConfig.<Integer, Object>builder().rateCache(RateCache.singleton()).build();
        return RateLimiter.<Integer>of(config, CompositeRate.of(Rate.of(limit, duration)));
    }
}