package com.looseboxes.ratelimiter;

import com.looseboxes.ratelimiter.bandwidths.Bandwidth;
import com.looseboxes.ratelimiter.bandwidths.Bandwidths;
import com.looseboxes.ratelimiter.util.Operator;
import com.looseboxes.ratelimiter.util.SleepingTicker;

import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.lang.Math.max;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * A rate limiter. Conceptually, a rate limiter distributes permits at a configurable rate. Each
 * {@link #acquire()} blocks if necessary until a permit is available, and then takes it. Once
 * acquired, permits need not be released.
 *
 * <p>{@code RateLimiter} is safe for concurrent use: It will restrict the total rate of calls from
 * all threads. Note, however, that it does not guarantee fairness.
 *
 * <p>Rate limiters are often used to restrict the rate at which some physical or logical resource
 * is accessed. This is in contrast to {@link java.util.concurrent.Semaphore} which restricts the
 * number of concurrent accesses instead of the rate (note though that concurrency and rate are
 * closely related, e.g. see <a href="http://en.wikipedia.org/wiki/Little%27s_law">Little's
 * Law</a>).
 *
 * <p>A {@code RateLimiter} is defined primarily by the rate at which permits are issued. Absent
 * additional configuration, permits will be distributed at a fixed rate, defined in terms of
 * permits per second. Permits will be distributed smoothly, with the delay between individual
 * permits being adjusted to ensure that the configured rate is maintained.
 *
 * <p>It is possible to configure a {@code RateLimiter} to have a warmup period during which time
 * the permits issued each second steadily increases until it hits the stable rate.
 *
 * <p>As an example, imagine that we have a list of tasks to execute, but we don't want to submit
 * more than 2 per second:
 *
 * <pre>{@code
 * final RateLimiter rateLimiter = RateLimiter.create(2.0); // rate is "2 permits per second"
 * void submitTasks(List<Runnable> tasks, Executor executor) {
 *   for (Runnable task : tasks) {
 *     rateLimiter.acquire(); // may wait
 *     executor.execute(task);
 *   }
 * }
 * }</pre>
 *
 * <p>As another example, imagine that we produce a stream of data, and we want to cap it at 5kb per
 * second. This could be accomplished by requiring a permit per byte, and specifying a rate of 5000
 * permits per second:
 *
 * <pre>{@code
 * final RateLimiter rateLimiter = RateLimiter.create(5000.0); // rate = 5000 permits per second
 * void submitPacket(byte[] packet) {
 *   rateLimiter.acquire(packet.length);
 *   networkService.send(packet);
 * }
 * }</pre>
 *
 * <p>It is important to note that the number of permits requested <i>never</i> affects the
 * throttling of the request itself (an invocation to {@code acquire(1)} and an invocation to {@code
 * acquire(1000)} will result in exactly the same throttling, if any), but it affects the throttling
 * of the <i>next</i> request. I.e., if an expensive task arrives at an idle RateLimiter, it will be
 * granted immediately, but it is the <i>next</i> request that will experience extra throttling,
 * thus paying for the cost of the expensive task.
 *
 * @author Dimitris Andreou
 * @since 13.0
 */
// TODO(user): switch to nano precision. A natural unit of cost is "bytes", and a micro precision
// would mean a maximum rate of "1MB/s", which might be small in some cases.
final class SmoothBandwidthLimiter extends BandwidthLimiter {

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

    SmoothBandwidthLimiter(Bandwidths bandwidths, SleepingTicker ticker) {
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
    //@VisibleForTesting
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
     * Returns the stable rates (as {@code permits per seconds}) with which each {@code Bandwidth} in this
     * {@code BandwidthLimiter} is configured with. The initial value of each, is the same as the {@code permitsPerSecond}
     * argument passed in the factory method that produced each {@code Bandwidth} of this {@code BandwithLimiter}.
     */
    @Override
    public final double [] getPermitsPerSecond() {
        synchronized (mutex()) {
            return bandwidths.getPermitsPerSecond();
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
            return reserveAndGetWaitLength(permits, ticker.elapsedMicros());
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
            if (!canAcquire(nowMicros, timeoutMicros)) {
                return false;
            }
            long microsToWait = reserveAndGetWaitLength(permits, nowMicros);
            ticker.sleepMicrosUninterruptibly(microsToWait);
            return true;
        }
    }

    private boolean canAcquire(long nowMicros, long timeoutMicros) {
        int failureCount = 0;
        for (Bandwidth bandwidth : bandwidths.getMembers()) {
            if (!canAcquire(bandwidth, nowMicros, timeoutMicros)) {
                ++failureCount;
            }
        }
        return !bandwidths.isExceeded(failureCount);
    }

    private boolean canAcquire(Bandwidth bandwidth, long nowMicros, long timeoutMicros) {
        return bandwidth.microsTillNextAvailable(nowMicros) - timeoutMicros <= nowMicros;
    }

    /**
     * Reserves next ticket and returns the wait time that the caller must wait for.
     *
     * @return the required wait time, never negative
     */
    final long reserveAndGetWaitLength(int permits, long nowMicros) {
        final boolean all = Operator.AND.equals(bandwidths.getOperator());
        long waitTime = 0;
        for(Bandwidth bandwidth : bandwidths.getMembers()) {
            final long thisWaitTime = reserveAndGetWaitLength(bandwidth, permits, nowMicros);
            if (all && thisWaitTime > waitTime) {
                waitTime = thisWaitTime;
                continue;
            }
            if (!all && (waitTime == 0 || thisWaitTime < waitTime)) {
                waitTime = thisWaitTime;
            }
        }
        return waitTime;
    }

    final long reserveAndGetWaitLength(Bandwidth bandwidth, int permits, long nowMicros) {
        final long momentAvailable = bandwidth.reserveNextAvailable(permits, nowMicros);
        return max(momentAvailable - nowMicros, 0);
    }

    @Override
    public String toString() {
        final double [] permitsPerSecond = getPermitsPerSecond();
        final StringBuilder builder = new StringBuilder(77 + (permitsPerSecond.length * 7));
        builder.append("SmoothBandwidthLimiter{stableRates/second=");
        for(int i=0; i<permitsPerSecond.length; i++) {
            builder.append(String.format(Locale.ROOT, "%3.1f", permitsPerSecond[i]));
            if (i < permitsPerSecond.length - 1) {
                builder.append(", ");
            }
        }
        return builder.append("}]").toString();
    }

    private static void checkPermits(int permits) {
        Checks.requireTrue(permits > 0, "Requested permits (%s) must be positive", permits);
    }
}
