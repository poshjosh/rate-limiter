package com.looseboxes.ratelimiter.wip.bandwidth;

public interface Bandwidth {

    void setRate(double permitsPerSecond, long nowMicros);

    double getRate();

    /**
     * Returns the earliest time that permits are available (with one caveat).
     *
     * @return the time that permits are available, or, if permits are available immediately, an
     *     arbitrary past or present time
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
}
