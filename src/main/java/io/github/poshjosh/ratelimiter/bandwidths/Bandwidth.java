package io.github.poshjosh.ratelimiter.bandwidths;

import java.util.concurrent.TimeUnit;

import static java.lang.Math.max;

/**
 * A Bandwidth is quantity that can be used over a period of time.
 * Bandwidths change with use and are created from rates.
 * Rates on the other hand specify the scope/limit of each bandwidth.
 * @see io.github.poshjosh.ratelimiter.model.Rate
 */
public interface Bandwidth {

    default boolean isAvailable(long nowMicros) {
        return isAvailable(nowMicros, 0);
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
    default double getPermitsPerSecond() {
        return getPermitsPer(TimeUnit.SECONDS);
    }

    double getPermitsPer(TimeUnit timeUnit);

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
