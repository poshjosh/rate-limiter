package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.annotations.Beta;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

import static java.lang.Math.max;
import static java.util.concurrent.TimeUnit.SECONDS;

public interface Bandwidth {

    Bandwidth UNLIMITED = new Bandwidth() {
        @Override public Bandwidth with(long nowMicros) { return this; }
        @Override public double getPermitsPerSecond() { return Double.MAX_VALUE; }
        @Override public long queryEarliestAvailable(long now) { return now; }
        @Override public long reserveEarliestAvailable(int permits, long now) { return now; }

        @Override public String toString() {
            return "Bandwidth$UNLIMITED";
        }
    };

    /** Beta */
    @Beta
    static Bandwidth allOrNothing(long permitsPerSecond) {
        return allOrNothing(permitsPerSecond, Duration.ofSeconds(1));
    }

    /** Beta */
    @Beta
    static Bandwidth allOrNothing(long permits, Duration duration) {
        return allOrNothing(permits, duration, 0);
    }

    /** Beta */
    @Beta
    static Bandwidth allOrNothing(long permits, Duration duration, long nowMicros) {
        return allOrNothing(permits, duration.toNanos(), TimeUnit.NANOSECONDS, nowMicros);
    }

    /**
     * Creates an {@code AllOrNothingBandwidth}, wired to return a value between zero and the stable interval for
     * {@link #queryEarliestAvailable(long)} and {@link #reserveEarliestAvailable(int, long)}
     *
     * Beta
     */
    @Beta
    static Bandwidth allOrNothing(long permits, long duration, TimeUnit timeUnit, long nowMicros) {
        return new AllOrNothingBandwidth(permits, duration, timeUnit, nowMicros);
    }

    static Bandwidth bursty(double permitsPerSecond) {
        return bursty(permitsPerSecond, 0);
    }

    static Bandwidth bursty(double permitsPerSecond, long nowMicros) {
        /*
         * The default bursty Bandwidth configuration can save the unused permits of up to one second. This
         * is to avoid unnecessary stalls in situations like this: A Bandwidth of 1qps, and 4 threads,
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
        return bursty(permitsPerSecond, nowMicros, 1.0);
    }

    /**
     * @see SmoothBandwidth#bursty(double, long, double)
     */
    static Bandwidth bursty(double permitsPerSecond, long nowMicros, double maxBurstSeconds) {
        return SmoothBandwidth.bursty(permitsPerSecond, nowMicros, maxBurstSeconds);
    }

    static Bandwidth warmingUp(double permitsPerSecond) {
        return warmingUp(permitsPerSecond, 1);
    }

    static Bandwidth warmingUp(double permitsPerSecond, long warmupPeriodSeconds) {
        return warmingUp(permitsPerSecond, 0, warmupPeriodSeconds);
    }

    static Bandwidth warmingUp(double permitsPerSecond, long nowMicros, long warmupPeriodSeconds) {
        return warmingUp(permitsPerSecond, nowMicros, warmupPeriodSeconds, SECONDS, 3.0);
    }

    /**
     * @see SmoothBandwidth#warmingUp(double, long, long, TimeUnit, double)
     */
    static Bandwidth warmingUp(double permitsPerSecond, long nowMicros,
                                            long warmupPeriod, TimeUnit timeUnit, double coldFactor) {
        return SmoothBandwidth.warmingUp(permitsPerSecond, nowMicros, warmupPeriod, timeUnit, coldFactor);
    }

    /**
     * @deprecated Rather use {code #isAvailable(long, long)}
     * @param nowMicros
     * @param timeoutMicros
     * @return true if the requested number of permits can be obtained within the specified timeout
     */
    @Deprecated
    default boolean canAcquire(long nowMicros, long timeoutMicros) {
        return isAvailable(nowMicros, timeoutMicros);
    }

    default boolean isAvailable(long nowMicros, long timeoutMicros) {
        final long nextFreeTicketAvailableAt = queryEarliestAvailable(nowMicros);
        return nextFreeTicketAvailableAt - timeoutMicros <= nowMicros;
    }

    default long reserveAndGetWaitLength(int permits, long nowMicros) {
        final long nextFreeTickerAvailableAt = reserveEarliestAvailable(permits, nowMicros);
        return max(nextFreeTickerAvailableAt - nowMicros, 0);
    }

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which this {@code Bandwidth} is
     * configured with. The initial value of this is the same as the {@code permitsPerSecond} argument
     * passed in the factory method that produced this {@code Bandwidth}.
     */
    double getPermitsPerSecond();

    /**
     * Returns the earliest time that permits are available (with one caveat).
     * <p>
     * This operation must not lead to the modification of the Bandwidth.
     *
     * @return the time that permits are available, or, if permits are available immediately, an
     * arbitrary past or present time
     */
    long queryEarliestAvailable(long nowMicros);

    /**
     * Reserves the requested number of permits and returns the time that those permits can be used
     * (with one caveat).
     *
     * @return the time that the permits may be used, or, if the permits may be used immediately, an
     *     arbitrary past or present time
     */
    long reserveEarliestAvailable(int permits, long nowMicros);

    /**
     * Return a copy of this Bandwidth with the specified time.
     * @param nowMicros The time to set.
     * @return A copy of this Bandwidth with the specified time.
     */
    Bandwidth with(long nowMicros);
}
