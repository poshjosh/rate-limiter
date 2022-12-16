package com.wip.ratelimiter.rate;

import java.util.concurrent.TimeUnit;

import static java.lang.Math.min;

final class SmoothWarmingUpRate extends SmoothRate {

    private final long warmupPeriodMicros;
    private final double coldFactor;
    /**
     * The slope of the line from the stable interval (when permits == 0), to the cold interval
     * (when permits == maxPermits)
     */
    private double slope;

    private double thresholdPermits;

    SmoothWarmingUpRate(double permitsPerSecond, long nowMicros,
            long warmupPeriod, TimeUnit timeUnit, double coldFactor) {
        this.warmupPeriodMicros = timeUnit.toMicros(warmupPeriod); // Before calling setPermitsPerSecond
        this.coldFactor = coldFactor; // Before calling setPermitsPerSecond
        setRate(permitsPerSecond, nowMicros);
    }

    public SmoothWarmingUpRate with(double permitsPerSecond, long nowMicros) {
        return new SmoothWarmingUpRate(permitsPerSecond, nowMicros, warmupPeriodMicros, TimeUnit.MICROSECONDS, coldFactor);
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

    @Override
    public String toString() {
        return "SmoothWarmingUpRate{" + "storedPermits=" + storedPermits + ", maxPermits="
                + maxPermits + ", stableIntervalMicros=" + stableIntervalMicros
                + ", warmupPeriodMicros=" + warmupPeriodMicros + ", slope=" + slope
                + ", thresholdPermits=" + thresholdPermits + ", coldFactor=" + coldFactor
                + ", nextFreeTicketMicros=" + getNextFreeTicketMicros() + '}';
    }
}
