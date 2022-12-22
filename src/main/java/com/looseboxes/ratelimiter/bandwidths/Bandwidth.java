package com.looseboxes.ratelimiter.bandwidths;

public interface Bandwidth {

    void setRate(double permitsPerSecond, long nowMicros);

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which this {@code Rate} is
     * configured with. The initial value of this is the same as the {@code permitsPerSecond} argument
     * passed in the factory method that produced this {@code Rate}.
     */
    double getRate();

    /**
     * Returns the earliest time that permits are available (with one caveat).
     *
     * @return the time that permits are available, or, if permits are available immediately, an
     *     arbitrary past or present time
     */
    long microsTillNextAvailable(long nowMicros);

    /**
     * Reserves the requested number of permits and returns the time that those permits can be used
     * (with one caveat).
     *
     * @return the time that the permits may be used, or, if the permits may be used immediately, an
     *     arbitrary past or present time
     */
    long reserveNextAvailable(int permits, long nowMicros);
}
