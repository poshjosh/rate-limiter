package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.annotations.VisibleForTesting;
import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.util.SleepingTicker;

import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.lang.Math.max;
import static java.util.concurrent.TimeUnit.SECONDS;

final class DefaultBandwidthLimiter implements BandwidthLimiter {

    private final Bandwidths bandwidths;

    /**
     * The underlying timer; used both to measure elapsed time and sleep as necessary. A separate
     * object to facilitate testing.
     */
    private final SleepingTicker ticker;

    // Can't be initialized in the constructor because mocks don't call the constructor.
    private volatile Object mutexDoNotUseDirectly;

    private Object mutex() {
        Object mutex = mutexDoNotUseDirectly;
        if (mutex == null) {
            synchronized (this) {
                mutex = mutexDoNotUseDirectly;
                if (mutex == null) {
                    mutexDoNotUseDirectly = mutex = new Object();
                }
            }
        }
        return mutex;
    }

    DefaultBandwidthLimiter(Bandwidths bandwidths, SleepingTicker ticker) {
        this.bandwidths = Objects.requireNonNull(bandwidths);
        this.ticker = Objects.requireNonNull(ticker);
    }

    /**
     * Updates the stable rate of this {@code RateLimiter}, that is, the {@code permitsPerSecond}
     * argument provided in the factory method that constructed the {@code RateLimiter}. Currently
     * throttled threads will <b>not</b> be awakened as a result of this invocation, thus they do not
     * observe the new rate; only subsequent requests will.
     *
     * <p>Note though that, since each request repays (by waiting, if necessary) the cost of the
     * <i>previous</i> request, this means that the very next request after an invocation to {@code
     * setPermitsPerSecond} will not be affected by the new rate; it will pay the cost of the previous request,
     * which is in terms of the previous rate.
     *
     * <p>The behavior of the {@code RateLimiter} is not modified in any other way, e.g. if the {@code
     * RateLimiter} was configured with a warmup period of 20 seconds, it still has a warmup period of
     * 20 seconds after this method invocation.
     *
     * @param permitsPerSecond the new stable rate of this {@code RateLimiter}
     * @throws IllegalArgumentException if {@code permitsPerSecond} is negative or zero
     */
    @VisibleForTesting
    BandwidthLimiter setRate(double... permitsPerSecond) {
        for (double d : permitsPerSecond) {
            Checks.requireTrue(d > 0.0
                    && !Double.isNaN(d), "rate must be positive");
        }
        synchronized (mutex()) {
            final Bandwidth [] members = bandwidths.getMembers();;
            for (int i = 0; i < members.length; i++) {
                members[i].setRate(permitsPerSecond[i], ticker.elapsedMicros());
            }
        }
        return this;
    }

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which the currently eligible {@code Bandwidth}
     * in this {@code BandwidthLimiter} is configured with. The initial value is same as the {@code permitsPerSecond}
     * argument passed in the factory method that produced the {@code Bandwidth}.
     */
    @Override
    public final double getPermitsPerSecond() {
        synchronized (mutex()) {
            return bandwidths.getRate();
        }
    }

    /**
     * Acquires the given number of permits from this {@code RateLimiter}, blocking until the request
     * can be granted. Tells the amount of time slept, if any.
     *
     * @param permits the number of permits to acquire
     * @return time spent sleeping to enforce rate, in seconds; 0.0 if not rate-limited
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     * @since 16.0 (present in 13.0 with {@code void} return type})
     */
    @Override
    public double acquire(int permits) {
        long microsToWait = reserve(permits);
        ticker.sleepMicrosUninterruptibly(microsToWait);
        return 1.0 * microsToWait / SECONDS.toMicros(1L);
    }

    /**
     * Reserves the given number of permits from this {@code RateLimiter} for future use, returning
     * the number of microseconds until the reservation can be consumed.
     *
     * @return time in microseconds to wait until the resource can be acquired, never negative
     */
    final long reserve(int permits) {
        checkPermits(permits);
        synchronized (mutex()) {
            return bandwidths.reserveAndGetWaitLength(permits, ticker.elapsedMicros());
        }
    }

    /**
     * Acquires the given number of permits from this {@code RateLimiter} if it can be obtained
     * without exceeding the specified {@code timeout}, or returns {@code false} immediately (without
     * waiting) if the permits would not have been granted before the timeout expired.
     *
     * @param permits the number of permits to acquire
     * @param timeout the maximum time to wait for the permits. Negative values are treated as zero.
     * @param unit the time unit of the timeout argument
     * @return {@code true} if the permits were acquired, {@code false} otherwise
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    @SuppressWarnings("GoodTime") // should accept a java.time.Duration
    @Override
    public boolean tryAcquire(int permits, long timeout, TimeUnit unit) {
        long timeoutMicros = max(unit.toMicros(timeout), 0);
        checkPermits(permits);
        synchronized (mutex()) {
            long nowMicros = ticker.elapsedMicros();
            if (!bandwidths.canAcquire(nowMicros, timeoutMicros)) {
                return false;
            }
            long microsToWait = bandwidths.reserveAndGetWaitLength(permits, nowMicros);
            ticker.sleepMicrosUninterruptibly(microsToWait);
            return true;
        }
    }

    @Override
    public String toString() {
        final Bandwidth [] members = bandwidths.getMembers();
        final StringBuilder builder = new StringBuilder(77 + (members.length * 7));
        builder.append("DefaultBandwidthLimiter{stableRates/second=");
        for(int i=0; i<members.length; i++) {
            builder.append(String.format(Locale.ROOT, "%3.1f", members[i].getRate()));
            if (i < members.length - 1) {
                builder.append(", ");
            }
        }
        return builder.append("}]").toString();
    }

    private static void checkPermits(int permits) {
        Checks.requireTrue(permits > 0, "Requested permits (%s) must be positive", permits);
    }
}
