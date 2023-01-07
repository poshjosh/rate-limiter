package io.github.poshjosh.ratelimiter;

import io.github.poshjosh.ratelimiter.annotations.VisibleForTesting;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidth;
import io.github.poshjosh.ratelimiter.bandwidths.Bandwidths;
import io.github.poshjosh.ratelimiter.util.SleepingTicker;

import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.lang.Math.max;
import static java.util.concurrent.TimeUnit.SECONDS;

final class DefaultRateLimiter implements RateLimiter {

    private final Bandwidths bandwidths;

    /**
     * The underlying timer; used both to measure elapsed time and sleep as necessary. A separate
     * object to facilitate testing.
     */
    private final SleepingTicker ticker;

    // Can't be initialized in the constructor because mocks don't call the constructor.
    private volatile Object mutexDoNotUseDirectly;

    Object mutex() {
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

    DefaultRateLimiter(Bandwidths bandwidths, SleepingTicker ticker) {
        this.bandwidths = Bandwidths.of(bandwidths);
        this.ticker = Objects.requireNonNull(ticker);
    }

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which the currently eligible {@code Bandwidth}
     * in this {@code RateLimiter} is configured with. The initial value is same as the {@code permitsPerSecond}
     * argument passed in the factory method that produced the {@code Bandwidth}.
     */
    @Override
    public final double getPermitsPerSecond() {
        synchronized (mutex()) {
            return bandwidths.getPermitsPerSecond();
        }
    }

    /**
     * Acquires the given number of permits from this {@code ResourceLimiter}, blocking until the request
     * can be granted. Tells the amount of time slept, if any.
     *
     * @param permits the number of permits to acquire
     * @return time spent sleeping to enforce rate, in seconds; 0.0 if not rate-limited
     * @throws IllegalArgumentException if the requested number of permits is negative or zero
     */
    @Override
    public double acquire(int permits) {
        long microsToWait = reserve(permits);
        ticker.sleepMicrosUninterruptibly(microsToWait);
        return 1.0 * microsToWait / SECONDS.toMicros(1L);
    }

    /**
     * Reserves the given number of permits from this {@code ResourceLimiter} for future use, returning
     * the number of microseconds until the reservation can be consumed.
     *
     * @return time in microseconds to wait until the resource can be acquired, never negative
     */
    final long reserve(int permits) {
        Checks.requirePositive(permits, "permits");
        synchronized (mutex()) {
            return bandwidths.reserveAndGetWaitLength(permits, ticker.elapsedMicros());
        }
    }

    /**
     * Acquires the given number of permits from this {@code ResourceLimiter} if it can be obtained
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
        Checks.requirePositive(permits, "permits");
        long timeoutMicros = max(unit.toMicros(timeout), 0);
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

    @VisibleForTesting
    Bandwidths getBandwidths() {
        return bandwidths;
    }

    @Override
    public String toString() {
        final Bandwidth[] members = bandwidths.getMembers();
        final StringBuilder builder = new StringBuilder(77 + (members.length * 7));
        builder.append("DefaultRateLimiter{stableRates/second=");
        for(int i=0; i<members.length; i++) {
            builder.append(String.format(Locale.ROOT, "%3.1f", members[i].getPermitsPerSecond()));
            if (i < members.length - 1) {
                builder.append(", ");
            }
        }
        return builder.append("}]").toString();
    }
}
