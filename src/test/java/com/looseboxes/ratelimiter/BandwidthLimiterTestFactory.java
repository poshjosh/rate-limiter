package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.bandwidths.SmoothBandwidth;
import com.looseboxes.ratelimiter.util.SleepingTicker;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

final class BandwidthLimiterTestFactory {
    private BandwidthLimiterTestFactory() { }

    /**
     * Creates a {@code RateLimiter} with the specified stable throughput, given as "permits per
     * second" (commonly referred to as <i>QPS</i>, queries per second).
     *
     * <p>The returned {@code RateLimiter} ensures that on average no more than {@code
     * permitsPerSecond} are issued during any given second, with sustained requests being smoothly
     * spread over each second. When the incoming request rate exceeds {@code permitsPerSecond} the
     * rate limiter will release one permit every {@code (1.0 / permitsPerSecond)} seconds. When the
     * rate limiter is unused, bursts of up to {@code permitsPerSecond} permits will be allowed, with
     * subsequent requests being smoothly limited at the stable rate of {@code permitsPerSecond}.
     *
     * @param permitsPerSecond the rate of the returned {@code RateLimiter}, measured in how many
     *     permits become available per second
     * @throws IllegalArgumentException if {@code permitsPerSecond} is negative or zero
     */
    static BandwidthLimiter create(double permitsPerSecond) {
        /*
         * The default RateLimiter configuration can save the unused permits of up to one second. This
         * is to avoid unnecessary stalls in situations like this: A RateLimiter of 1qps, and 4 threads,
         * all calling acquire() at these moments:
         *
         * T0 at 0 seconds
         * T1 at 1.05 seconds
         * T2 at 2 seconds
         * T3 at 3 seconds
         *
         * Due to the slight delay of T1, T2 would have to sleep till 2.05 seconds, and T3 would also
         * have to sleep till 3.05 seconds.
         */
        return create(permitsPerSecond, SleepingTicker.zeroOffset());
    }

    static BandwidthLimiter create(double permitsPerSecond, SleepingTicker ticker) {
        Bandwidth bandwidth = SmoothBandwidth.bursty(permitsPerSecond, ticker.elapsedMicros());
        return BandwidthLimiter.of(Bandwidths.of(bandwidth), ticker);
    }

    /**
     * Creates a {@code RateLimiter} with the specified stable throughput, given as "permits per
     * second" (commonly referred to as <i>QPS</i>, queries per second), and a <i>warmup period</i>,
     * during which the {@code RateLimiter} smoothly ramps up its rate, until it reaches its maximum
     * rate at the end of the period (as long as there are enough requests to saturate it). Similarly,
     * if the {@code RateLimiter} is left <i>unused</i> for a duration of {@code warmupPeriod}, it
     * will gradually return to its "cold" state, i.e. it will go through the same warming up process
     * as when it was first created.
     *
     * <p>The returned {@code RateLimiter} is intended for cases where the resource that actually
     * fulfills the requests (e.g., a remote server) needs "warmup" time, rather than being
     * immediately accessed at the stable (maximum) rate.
     *
     * <p>The returned {@code RateLimiter} starts in a "cold" state (i.e. the warmup period will
     * follow), and if it is left unused for long enough, it will return to that state.
     *
     * @param permitsPerSecond the rate of the returned {@code RateLimiter}, measured in how many
     *     permits become available per second
     * @param warmupPeriod the duration of the period where the {@code RateLimiter} ramps up its rate,
     *     before reaching its stable (maximum) rate
     * @throws IllegalArgumentException if {@code permitsPerSecond} is negative or zero or {@code
     *     warmupPeriod} is negative
     * @since 28.0
     */
    static BandwidthLimiter create(double permitsPerSecond, Duration warmupPeriod) {
        return create(permitsPerSecond, Util.toNanosSaturated(warmupPeriod), TimeUnit.NANOSECONDS);
    }

    /**
     * Creates a {@code RateLimiter} with the specified stable throughput, given as "permits per
     * second" (commonly referred to as <i>QPS</i>, queries per second), and a <i>warmup period</i>,
     * during which the {@code RateLimiter} smoothly ramps up its rate, until it reaches its maximum
     * rate at the end of the period (as long as there are enough requests to saturate it). Similarly,
     * if the {@code RateLimiter} is left <i>unused</i> for a duration of {@code warmupPeriod}, it
     * will gradually return to its "cold" state, i.e. it will go through the same warming up process
     * as when it was first created.
     *
     * <p>The returned {@code RateLimiter} is intended for cases where the resource that actually
     * fulfills the requests (e.g., a remote server) needs "warmup" time, rather than being
     * immediately accessed at the stable (maximum) rate.
     *
     * <p>The returned {@code RateLimiter} starts in a "cold" state (i.e. the warmup period will
     * follow), and if it is left unused for long enough, it will return to that state.
     *
     * @param permitsPerSecond the rate of the returned {@code RateLimiter}, measured in how many
     *     permits become available per second
     * @param warmupPeriod the duration of the period where the {@code RateLimiter} ramps up its rate,
     *     before reaching its stable (maximum) rate
     * @param unit the time unit of the warmupPeriod argument
     * @throws IllegalArgumentException if {@code permitsPerSecond} is negative or zero or {@code
     *     warmupPeriod} is negative
     */
    static BandwidthLimiter create(double permitsPerSecond, long warmupPeriod, TimeUnit unit) {
        Checks.requireTrue(warmupPeriod >= 0, "warmupPeriod must not be negative: %s", warmupPeriod);
        SleepingTicker ticker = SleepingTicker.zeroOffset();
        Bandwidth bandwidth = SmoothBandwidth
                .warmingUp(permitsPerSecond, ticker.elapsedMicros(), unit.toSeconds(warmupPeriod));
        return BandwidthLimiter.of(Bandwidths.of(bandwidth), ticker);
    }

    static BandwidthLimiter create(double permitsPerSecond, long warmupPeriod, TimeUnit unit,
                                   double coldFactor, SleepingTicker ticker) {
        Bandwidth bandwidth = SmoothBandwidth
                .warmingUp(permitsPerSecond, ticker.elapsedMicros(), warmupPeriod, unit, coldFactor);
        return BandwidthLimiter.of(Bandwidths.of(bandwidth), ticker);
    }
}
