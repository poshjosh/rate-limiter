package com.looseboxes.ratelimiter.bandwidths;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Objects;

/**
 * This implements a "bursty" {@link Bandwidth}, where storedPermits are translated to zero throttling.
 * The maximum number of permits that can be saved (when the Bandwidth is unused) is defined in
 * terms of time, in this sense: if a Bandwidth is 2qps, and this time is specified as 10
 * seconds, we can save up to 2 * 10 = 20 permits.
 */
final class SmoothBurstyBandwidth extends SmoothBandwidth implements Serializable {

    private static final long serialVersionUID = 9081726354000000070L;

    private final long nowMicros; // Only for serialization/copy purposes

    /** The work (permits) of how many seconds can be saved up if this RateLimiter is unused? */
    private final double maxBurstSeconds;

    SmoothBurstyBandwidth(double permitsPerSecond, long nowMicros, double maxBurstSeconds) {
        this.nowMicros = nowMicros;
        this.maxBurstSeconds = maxBurstSeconds; // Before calling setPermitsPerSecond
        setRate(permitsPerSecond, nowMicros);
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

    @Override public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        if (!super.equals(o))
            return false;
        SmoothBurstyBandwidth that = (SmoothBurstyBandwidth) o;
        return Double.compare(that.maxBurstSeconds, maxBurstSeconds) == 0;
    }

    @Override public int hashCode() {
        return Objects.hash(super.hashCode(), maxBurstSeconds);
    }

    @Override
    public String toString() {
        return "SmoothBurstyBandwidth{storedPermits=" + storedPermits + ", maxPermits=" + maxPermits
                + ", stableIntervalMicros=" + stableIntervalMicros + ", maxBurstSeconds="
                + maxBurstSeconds + ", nextFreeTicketMicros=" + getNextFreeTicketMicros() + '}';
    }

    private static class SecureSerializationProxy implements Serializable{

        private static final long serialVersionUID = 9081726354000000071L;

        private final double permitsPerSecond;
        private final long nowMicros;
        private final double maxBurstSeconds;

        public SecureSerializationProxy(SmoothBurstyBandwidth candidate){
            this.permitsPerSecond = candidate.getRate();
            this.nowMicros = candidate.nowMicros;
            this.maxBurstSeconds = candidate.maxBurstSeconds;
        }
        private Object readResolve() throws InvalidObjectException {
            return new SmoothBurstyBandwidth(permitsPerSecond, nowMicros, maxBurstSeconds);
        }
    }

    private Object writeReplace(){
        return new SecureSerializationProxy(this);
    }
    private void readObject(ObjectInputStream ois) throws InvalidObjectException {
        throw new InvalidObjectException("Secure proxy must be used for serialization");
    }
}
