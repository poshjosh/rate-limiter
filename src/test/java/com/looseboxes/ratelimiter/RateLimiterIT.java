package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.rates.LimitWithinDuration;
import com.looseboxes.ratelimiter.rates.Rate;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

public class RateLimiterIT {

    @Test
    public void testPerformance() {
        testPerformance(1_000, 100, 3_000_000);
        testPerformance(10_000, 160, 3_000_000);
        testPerformance(100_000, 160, 10_000_000);
        testPerformance(1_000_000, 320, 60_000_000);
    }

    private void testPerformance(int count, long maxTime, long maxMemory) {
        final long tb4 = System.currentTimeMillis();
        final long mb4 = Util.availableMemory();
        RateLimiter<Integer> rateLimiter = getRateLimiter(count + 1, 60_000);
        for(int i = 0; i < count; i++) {
            rateLimiter.record(i);
        }
        final long timeSpent = (System.currentTimeMillis() - tb4);
        final long memorySpent = Util.usedMemory(mb4);

        System.out.printf("Count: %s, Spent -> time: %d millis, memory: %d kb", count, timeSpent, (memorySpent/1000));
        assertThat(timeSpent).isLessThan(maxTime);
        assertThat(memorySpent).isLessThanOrEqualTo(maxMemory);
    }

    public RateLimiter<Integer> getRateLimiter(int limit, int duration) {
        Rate rateLimit = new LimitWithinDuration(limit, duration);
        return new DefaultRateLimiter<>(rateLimit);
    }
}

//Immutable Rate instances - SingletonRateLimiter

//Count: 1000, Spent -> time: 57 millis, memory: 2,694 kb
//Count: 1000, Spent -> time: 57 millis, memory: 2,694 kb

//Count: 10000, Spent -> time: 55 millis, memory: 2,694 kb
//Count: 10000, Spent -> time: 82 millis, memory: 2,694 kb

//Count: 100000, Spent -> time: 64 millis, memory: 8,084 kb
//Count: 100000, Spent -> time: 67 millis, memory: 8,084 kb

//Count: 1000000, Spent -> time: 153 millis, memory: 51,202 kb
//Count: 1000000, Spent -> time: 171 millis, memory: 51,202 kb

//Immutable Rate instances - DefaultRateLimiter

// Count: 1000000, Spent -> time: 292 millis, memory: 70,554 kb

// Mutable Rate instances SingletonRateLimiter

//Count: 1000, Spent -> time: 58 millis, memory: 2,695 kb
//Count: 1000, Spent -> time: 58 millis, memory: 2,694 kb

//Count: 10000, Spent -> time: 62 millis, memory: 2,694 kb
//Count: 10000, Spent -> time: 56 millis, memory: 2,694 kb

//Count: 100000, Spent -> time: 74 millis, memory: 5,389 kb
//Count: 100000, Spent -> time: 63 millis, memory: 5,389 kb

//Count: 1000000, Spent -> time: 131 millis, memory: 18,864 kb
//Count: 1000000, Spent -> time: 142 millis, memory: 18,864 kb

//Mutable Rate instances - DefaultRateLimiter

//Count: 1000000, Spent -> time: 417 millis, memory: 70363 kb