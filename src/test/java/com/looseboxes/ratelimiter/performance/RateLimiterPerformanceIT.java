package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.SmoothBandwidth;
import com.looseboxes.ratelimiter.cache.RateCache;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import org.junit.jupiter.api.Test;

class RateLimiterPerformanceIT {

    @Test
    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory() {

        // @TODO introduce maven profiles so performance tests could be INFO, while other tests DEBUG

        // These stats were achieved under log level INFO

        // 2 Nov 2021
        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(1_000_000, Usage.of(250, 50_000_000));
    }

    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(int count, Usage limit) {

        RateLimiter<Integer> rateLimiter = getRateLimiter((double)count + 1 / 60);

        Usage bookmark = Usage.bookmark();

        final Integer key = Integer.valueOf(count);
        for(int i = 0; i < count; i++) {
            rateLimiter.tryConsume(key);
        }

        bookmark.assertUsageLessThan(limit);
    }

    public RateLimiter<Integer> getRateLimiter(double permitsPerSecond) {
        RateLimiterConfig<Integer, ?> config =
            RateLimiterConfig.<Integer, Object>builder().rateCache(RateCache.singleton()).build();
        return RateLimiter.<Integer>of(config, Bandwidths.of(getBandwidth(permitsPerSecond)));
    }

    private Bandwidth getBandwidth(double permitsPerSecond) {
        return SmoothBandwidth.warmingUp(permitsPerSecond, 5);
    }
}