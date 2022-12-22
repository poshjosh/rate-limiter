package com.looseboxes.ratelimiter.bandwidths;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.lang.Math.min;

/**
 * This implements the following function where coldInterval = coldFactor * stableInterval.
 *
 * <pre>
 *          ^ throttling
 *          |
 *    cold  +                  /
 * interval |                 /.
 *          |                / .
 *          |               /  .   ← "warmup period" is the area of the trapezoid between
 *          |              /   .     thresholdPermits and maxPermits
 *          |             /    .
 *          |            /     .
 *          |           /      .
 *   stable +----------/  WARM .
 * interval |          .   UP  .
 *          |          . PERIOD.
 *          |          .       .
 *        0 +----------+-------+--------------→ storedPermits
 *          0 thresholdPermits maxPermits
 * </pre>
 *
 * Before going into the details of this particular function, let's keep in mind the basics:
 *
 * <ol>
 *   <li>The state of the Bandwidth (storedPermits) is a vertical line in this figure.
 *   <li>When the Bandwidth is not used, this goes right (up to maxPermits)
 *   <li>When the Bandwidth is used, this goes left (down to zero), since if we have
 *       storedPermits, we serve from those first
 *   <li>When _unused_, we go right at a constant rate! The rate at which we move to the right is
 *       chosen as maxPermits / warmupPeriod. This ensures that the time it takes to go from 0 to
 *       maxPermits is equal to warmupPeriod.
 *   <li>When _used_, the time it takes, as explained in the introductory class note, is equal to
 *       the integral of our function, between X permits and X-K permits, assuming we want to
 *       spend K saved permits.
 * </ol>
 *
 * <p>In summary, the time it takes to move to the left (spend K permits), is equal to the area of
 * the function of width == K.
 *
 * <p>Assuming we have saturated demand, the time to go from maxPermits to thresholdPermits is
 * equal to warmupPeriod. And the time to go from thresholdPermits to 0 is warmupPeriod/2. (The
 * reason that this is warmupPeriod/2 is to maintain the behavior of the original implementation
 * where coldFactor was hard coded as 3.)
 *
 * <p>It remains to calculate thresholdsPermits and maxPermits.
 *
 * <ul>
 *   <li>The time to go from thresholdPermits to 0 is equal to the integral of the function
 *       between 0 and thresholdPermits. This is thresholdPermits * stableIntervals. By (5) it is
 *       also equal to warmupPeriod/2. Therefore
 *       <blockquote>
 *       thresholdPermits = 0.5 * warmupPeriod / stableInterval
 *       </blockquote>
 *   <li>The time to go from maxPermits to thresholdPermits is equal to the integral of the
 *       function between thresholdPermits and maxPermits. This is the area of the pictured
 *       trapezoid, and it is equal to 0.5 * (stableInterval + coldInterval) * (maxPermits -
 *       thresholdPermits). It is also equal to warmupPeriod, so
 *       <blockquote>
 *       maxPermits = thresholdPermits + 2 * warmupPeriod / (stableInterval + coldInterval)
 *       </blockquote>
 * </ul>
 */
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
