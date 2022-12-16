package com.wip.ratelimiter.rate;

import com.wip.ratelimiter.Checks;

import java.util.concurrent.TimeUnit;

import static java.lang.Math.min;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;

public abstract class SmoothRate implements Rate {

    public static Rate bursty(double permitsPerSecond, long nowMicros) {
        return bursty(permitsPerSecond, nowMicros, 1.0);
    }

    public static Rate bursty(double permitsPerSecond, long nowMicros, double maxBurstSeconds) {
        return new SmoothBurstyRate(permitsPerSecond, nowMicros, maxBurstSeconds);
    }

    public static Rate warmingUp(double permitsPerSecond, long nowMicros, long warmupPeriodMicros) {
        return warmingUp(permitsPerSecond, nowMicros, warmupPeriodMicros, MICROSECONDS, 3.0);
    }

    public static Rate warmingUp(double permitsPerSecond, long nowMicros,
            long warmupPeriod, TimeUnit timeUnit, double coldFactor) {
        return new SmoothWarmingUpRate(permitsPerSecond, nowMicros, warmupPeriod, timeUnit, coldFactor);
    }

    /** The currently stored permits. */
    double storedPermits;

    /** The maximum number of stored permits. */
    double maxPermits;

    /**
     * The interval between two unit requests, at our stable rate. E.g., a stable rate of 5 permits
     * per second has a stable interval of 200ms.
     */
    double stableIntervalMicros;

    /**
     * The time when the next request (no matter its size) will be granted. After granting a request,
     * this is pushed further in the future. Large requests push this further than small requests.
     */
    private long nextFreeTicketMicros = 0L; // could be either in the past or future

    public abstract SmoothRate with(double permitsPerSecond, long nowMicros);

    protected abstract void doSetRate(double permitsPerSecond, double stableIntervalMicros);

    @Override
    public void setRate(double permitsPerSecond, long nowMicros) {
        Checks.requireTrue(permitsPerSecond > 0.0
                && !Double.isNaN(permitsPerSecond), "rate must be positive");
        resync(nowMicros);
        double stableIntervalMicros = SECONDS.toMicros(1L) / permitsPerSecond;
        this.stableIntervalMicros = stableIntervalMicros;
        doSetRate(permitsPerSecond, stableIntervalMicros);
    }

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which this {@code Rate} is
     * configured with. The initial value of this is the same as the {@code permitsPerSecond} argument
     * passed in the factory method that produced this {@code Rate}.
     */
    @Override
    public final double getRate() {
        return SECONDS.toMicros(1L) / stableIntervalMicros;
    }

    @Override
    public final long queryEarliestAvailable(long nowMicros) {
        return nextFreeTicketMicros;
    }

    @Override
    public final long reserveEarliestAvailable(int requiredPermits, long nowMicros) {
        resync(nowMicros);
        long returnValue = nextFreeTicketMicros;
        double storedPermitsToSpend = min(requiredPermits, this.storedPermits);
        double freshPermits = requiredPermits - storedPermitsToSpend;
        long waitMicros =
                storedPermitsToWaitTime(this.storedPermits, storedPermitsToSpend)
                        + (long) (freshPermits * stableIntervalMicros);

        this.nextFreeTicketMicros = addIgnoringSaturation(nextFreeTicketMicros, waitMicros);
        this.storedPermits -= storedPermitsToSpend;
        return returnValue;
    }

    /**
     * Returns the sum of {@code a} and {@code b} unless it would overflow or underflow in which case
     * {@code Long.MAX_VALUE} or {@code Long.MIN_VALUE} is returned, respectively.
     *
     * @since 20.0
     */
    private static long addIgnoringSaturation(long a, long b) {
        long naiveSum = a + b;
        if ((a ^ b) < 0 | (a ^ naiveSum) >= 0) {
            // If a and b have different signs or a has the same sign as the result then there was no
            // overflow, return.
            return naiveSum;
        }
        // we did over/under flow, if the sign is negative we should return MAX otherwise MIN
        return Long.MAX_VALUE + ((naiveSum >>> (Long.SIZE - 1)) ^ 1);
    }


    /**
     * Translates a specified portion of our currently stored permits which we want to spend/acquire,
     * into a throttling time. Conceptually, this evaluates the integral of the underlying function we
     * use, for the range of [(storedPermits - permitsToTake), storedPermits].
     *
     * <p>This always holds: {@code 0 <= permitsToTake <= storedPermits}
     */
    abstract long storedPermitsToWaitTime(double storedPermits, double permitsToTake);

    /**
     * Returns the number of microseconds during cool down that we have to wait to get a new permit.
     */
    abstract double coolDownIntervalMicros();


    /** Updates {@code storedPermits} and {@code nextFreeTicketMicros} based on the current time. */
    void resync(long nowMicros) {
        // if nextFreeTicket is in the past, resync to now
        if (nowMicros > nextFreeTicketMicros) {
            double newPermits = (nowMicros - nextFreeTicketMicros) / coolDownIntervalMicros();
            storedPermits = min(maxPermits, storedPermits + newPermits);
            nextFreeTicketMicros = nowMicros;
        }
    }

    protected long getNextFreeTicketMicros() {
        return nextFreeTicketMicros;
    }

    @Override
    public String toString() {
        return "SmoothRate{" + "storedPermits=" + storedPermits + ", maxPermits="
                + maxPermits + ", stableIntervalMicros=" + stableIntervalMicros
                + ", nextFreeTicketMicros=" + nextFreeTicketMicros + '}';
    }
}