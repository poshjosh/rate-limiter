package io.github.poshjosh.ratelimiter.bandwidths;

public interface BandwidthState {
    /**
     * Returns the stable rate (as {@code permits per seconds}) with which this {@code Bandwidth} is
     * configured with. The initial value of this is the same as the {@code permitsPerSecond} argument
     * passed in the factory method that produced this {@code Bandwidth}.
     */
    double getPermitsPerSecond();

    boolean isAvailable(long nowMicros, long timeoutMicros);

    /**
     * Returns the earliest time that permits are available (with one caveat).
     * <p>
     * This operation must not lead to the modification of the Bandwidth.
     *
     * @return the time that permits are available, or, if permits are available immediately, an
     * arbitrary past or present time
     */
    long queryEarliestAvailable(long nowMicros);
}
