package com.looseboxes.ratelimiter.bandwidths;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.lang.Math.min;

final class SmoothWarmingUpBandwidth extends SmoothBandwidth implements Serializable{

    private static final long serialVersionUID = 9081726354000000080L;

    private final long nowMicros; // Only for serialization/copy purposes
    private final long warmupPeriodMicros;
    private final double coldFactor;
    /**
     * The slope of the line from the stable interval (when permits == 0), to the cold interval
     * (when permits == maxPermits)
     */
    private double slope;

    private double thresholdPermits;

    SmoothWarmingUpBandwidth(double permitsPerSecond, long nowMicros,
            long warmupPeriod, TimeUnit timeUnit, double coldFactor) {
        this.nowMicros = nowMicros;
        this.warmupPeriodMicros = timeUnit.toMicros(warmupPeriod); // Before calling setPermitsPerSecond
        this.coldFactor = coldFactor; // Before calling setPermitsPerSecond
        setRate(permitsPerSecond, nowMicros);
    }

    private SmoothWarmingUpBandwidth(SmoothWarmingUpBandwidth smoothWarmingUpBandwidth) {
        super(smoothWarmingUpBandwidth);
        this.nowMicros = smoothWarmingUpBandwidth.nowMicros;
        this.warmupPeriodMicros = smoothWarmingUpBandwidth.warmupPeriodMicros;
        this.coldFactor = smoothWarmingUpBandwidth.coldFactor;
        this.slope = smoothWarmingUpBandwidth.slope;
        this.thresholdPermits = smoothWarmingUpBandwidth.thresholdPermits;
    }

    @Override
    public SmoothWarmingUpBandwidth copy() {
        return new SmoothWarmingUpBandwidth(this);
    }

    @Override
    protected void doSetRate(double permitsPerSecond, double stableIntervalMicros) {
        double oldMaxPermits = maxPermits;
        double coldIntervalMicros = stableIntervalMicros * coldFactor;
        thresholdPermits = 0.5 * warmupPeriodMicros / stableIntervalMicros;
        maxPermits =
                thresholdPermits + 2.0 * warmupPeriodMicros / (stableIntervalMicros + coldIntervalMicros);
        slope = (coldIntervalMicros - stableIntervalMicros) / (maxPermits - thresholdPermits);
        if (oldMaxPermits == Double.POSITIVE_INFINITY) {
            // if we don't special-case this, we would get storedPermits == NaN, below
            storedPermits = 0.0;
        } else {
            storedPermits =
                    (oldMaxPermits == 0.0)
                            ? maxPermits // initial state is cold
                            : storedPermits * maxPermits / oldMaxPermits;
        }
    }

    @Override
    long storedPermitsToWaitTime(double storedPermits, double permitsToTake) {
        double availablePermitsAboveThreshold = storedPermits - thresholdPermits;
        long micros = 0;
        // measuring the integral on the right part of the function (the climbing line)
        if (availablePermitsAboveThreshold > 0.0) {
            double permitsAboveThresholdToTake = min(availablePermitsAboveThreshold, permitsToTake);
            // TODO(cpovirk): Figure out a good name for this variable.
            double length =
                    permitsToTime(availablePermitsAboveThreshold)
                            + permitsToTime(availablePermitsAboveThreshold - permitsAboveThresholdToTake);
            micros = (long) (permitsAboveThresholdToTake * length / 2.0);
            permitsToTake -= permitsAboveThresholdToTake;
        }
        // measuring the integral on the left part of the function (the horizontal line)
        micros += (long) (stableIntervalMicros * permitsToTake);
        return micros;
    }

    private double permitsToTime(double permits) {
        return stableIntervalMicros + permits * slope;
    }

    @Override
    double coolDownIntervalMicros() {
        return warmupPeriodMicros / maxPermits;
    }

    @Override public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        if (!super.equals(o))
            return false;
        SmoothWarmingUpBandwidth that = (SmoothWarmingUpBandwidth) o;
        return warmupPeriodMicros == that.warmupPeriodMicros
                && Double.compare(that.coldFactor, coldFactor) == 0
                && Double.compare(that.slope, slope) == 0
                && Double.compare(that.thresholdPermits, thresholdPermits) == 0;
    }

    @Override public int hashCode() {
        return Objects
                .hash(super.hashCode(), warmupPeriodMicros, coldFactor, slope, thresholdPermits);
    }

    @Override
    public String toString() {
        return "SmoothWarmingUpBandwidth{storedPermits=" + storedPermits + ", maxPermits="
                + maxPermits + ", stableIntervalMicros=" + stableIntervalMicros
                + ", warmupPeriodMicros=" + warmupPeriodMicros + ", slope=" + slope
                + ", thresholdPermits=" + thresholdPermits + ", coldFactor=" + coldFactor
                + ", nextFreeTicketMicros=" + getNextFreeTicketMicros() + '}';
    }

    private static class SerializationProxy implements Serializable {

        private static final long serialVersionUID = 9081726354000000081L;

        private final double permitsPerSecond;
        private final long nowMicros; // Only for serialization/copy purposes
        private final long warmupPeriodMicros;
        private final double coldFactor;

        public SerializationProxy(SmoothWarmingUpBandwidth candidate){
            this.permitsPerSecond = candidate.getRate();
            this.nowMicros = candidate.nowMicros;
            this.warmupPeriodMicros = candidate.warmupPeriodMicros;
            this.coldFactor = candidate.coldFactor;
        }
        private Object readResolve() throws InvalidObjectException {
            return new SmoothWarmingUpBandwidth(
                    permitsPerSecond, nowMicros, warmupPeriodMicros, TimeUnit.MICROSECONDS, coldFactor);
        }
    }

    private Object writeReplace(){
        return new SmoothWarmingUpBandwidth.SerializationProxy(this);
    }
    private void readObject(ObjectInputStream ois) throws InvalidObjectException {
        throw new InvalidObjectException("Secure proxy must be used for serialization");
    }
}
