package com.looseboxes.ratelimiter.performance;

import com.looseboxes.ratelimiter.*;
import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.cache.RateCache;
import org.junit.jupiter.api.Test;

class ResourceLimiterPerformanceIT {

    @Test
    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory() {

        // @TODO introduce maven profiles so performance tests could be INFO, while other tests DEBUG

        // These stats were achieved under log level INFO

        recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(1_000, 20_000, Usage.of(250, 50_000_000));
    }

    void recordMethodInvocationsShouldConsumeLimitedTimeAndMemory(int count, int permitsPerSecond, Usage limit) {

        ResourceLimiter<Integer> resourceLimiter = getResourceLimiter(permitsPerSecond);

        Usage bookmark = Usage.bookmark();

        final Integer key = Integer.valueOf(count);
        for(int i = 0; i < count; i++) {
            resourceLimiter.tryConsume(key);
        }

        bookmark.assertUsageLessThan(limit);
    }

    public ResourceLimiter<Integer> getResourceLimiter(long permitsPerSecond) {
        return ResourceLimiter.<Integer>of(Bandwidth.bursty(permitsPerSecond))
                .cache(RateCache.ofMap());
    }
}