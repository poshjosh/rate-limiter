package com.looseboxes.ratelimiter.wip.bandwidths;

final class SmoothBurstyBandwidth extends SmoothBandwidth {

    /** The work (permits) of how many seconds can be saved up if this RateLimiter is unused? */
    private final double maxBurstSeconds;

    SmoothBurstyBandwidth(double permitsPerSecond, long nowMicros, double maxBurstSeconds) {
        this.maxBurstSeconds = maxBurstSeconds; // Before calling setPermitsPerSecond
        setRate(permitsPerSecond, nowMicros);
    }

    public SmoothBurstyBandwidth with(double permitsPerSecond, long nowMicros) {
        return new SmoothBurstyBandwidth(permitsPerSecond, nowMicros, maxBurstSeconds);
    }

    @Override
    protected void doSetRate(double permitsPerSecond, double stableIntervalMicros) {
        double oldMaxPermits = this.maxPermits;
        maxPermits = maxBurstSeconds * permitsPerSecond;
        if (oldMaxPermits == Double.POSITIVE_INFINITY) {
            // if we don't special-case this, we would get storedPermits == NaN, below
            storedPermits = maxPermits;
        } else {
            storedPermits =
                    (oldMaxPermits == 0.0)
                            ? 0.0 // initial state
                            : storedPermits * maxPermits / oldMaxPermits;
        }
    }

    @Override
    long storedPermitsToWaitTime(double storedPermits, double permitsToTake) {
        return 0L;
    }

    @Override
    double coolDownIntervalMicros() {
        return stableIntervalMicros;
    }

    @Override
    public String toString() {
        return "SmoothBurstyRate{" + "storedPermits=" + storedPermits + ", maxPermits=" + maxPermits
                + ", stableIntervalMicros=" + stableIntervalMicros + ", maxBurstSeconds="
                + maxBurstSeconds + ", nextFreeTicketMicros=" + getNextFreeTicketMicros() + '}';
    }
}
