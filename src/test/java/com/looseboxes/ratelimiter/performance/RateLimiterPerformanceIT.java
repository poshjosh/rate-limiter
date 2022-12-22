package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.util.Rate;
import com.looseboxes.ratelimiter.util.Rates;
import org.junit.jupiter.api.Test;

import java.time.Duration;

class RateLimiterPerformanceIT {

    @Test
    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory() {

        // @TODO introduce maven profiles so performance tests could be INFO, while other tests DEBUG

        // These stats were achieved under log level INFO

        // 2 Nov 2021
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(1_000_000, 20_000, Usage.of(250, 50_000_000));
    }

    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(int count, int permitsPerSecond, Usage limit) {

        RateLimiter<Integer> rateLimiter = getRateLimiter(permitsPerSecond);

        Usage bookmark = Usage.bookmark();

        final Integer key = Integer.valueOf(count);
        for(int i = 0; i < count; i++) {
            rateLimiter.tryConsume(key);
        }

        bookmark.assertUsageLessThan(limit);
    }

    public RateLimiter<Integer> getRateLimiter(long permitsPerSecond) {
        RateLimiterConfig<Integer, ?> config =
            RateLimiterConfig.<Integer, Object>builder().rateCache(RateCache.singleton()).build();
        return RateLimiter.<Integer>of(config, Rates.of(Rate.of(permitsPerSecond, Duration.ofSeconds(1))));
    }
}