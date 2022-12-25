package com.looseboxes.ratelimiter.bandwidths;

import static java.lang.Math.max;

public interface Bandwidth {

    void setRate(double permitsPerSecond, long nowMicros);

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which this {@code Rate} is
     * configured with. The initial value of this is the same as the {@code permitsPerSecond} argument
     * passed in the factory method that produced this {@code Rate}.
     */
    double getRate();

    default boolean canAcquire(long nowMicros, long timeoutMicros) {
        final long earliestAvailable = queryEarliestAvailable(nowMicros);
        return earliestAvailable - timeoutMicros <= nowMicros;
    }

    /**
     * Returns the earliest time that permits are available (with one caveat).
     *
     * @return the time that permits are available, or, if permits are available immediately, an
     *     arbitrary past or present time
     */
    long queryEarliestAvailable(long nowMicros);

    default long reserveAndGetWaitLength(int permits, long nowMicros) {
        final long momentAvailable = reserveEarliestAvailable(permits, nowMicros);
        return max(momentAvailable - nowMicros, 0);
    }

    /**
     * Reserves the requested number of permits and returns the time that those permits can be used
     * (with one caveat).
     *
     * @return the time that the permits may be used, or, if the permits may be used immediately, an
     *     arbitrary past or present time
     */
    long reserveEarliestAvailable(int permits, long nowMicros);
}
