package com.looseboxes.ratelimiter.bandwidths;

import com.looseboxes.ratelimiter.Checks;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.lang.Math.min;
import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * How is the SmoothBandwidth designed, and why?
 *
 * The primary feature of a SmoothBandwidth is its "stable rate", the maximum rate that it should
 * allow in normal conditions. This is enforced by "throttling" incoming requests as needed. For
 * example, we could compute the appropriate throttle time for an incoming request, and make the
 * calling thread wait for that time.
 *
 * The simplest way to maintain a rate of QPS is to keep the timestamp of the last granted
 * request, and ensure that (1/QPS) seconds have elapsed since then. For example, for a rate of
 * QPS=5 (5 tokens per second), if we ensure that a request isn't granted earlier than 200ms after
 * the last one, then we achieve the intended rate. If a request comes and the last request was
 * granted only 100ms ago, then we wait for another 100ms. At this rate, serving 15 fresh permits
 * (i.e. for an acquire(15) request) naturally takes 3 seconds.
 *
 * It is important to realize that such a SmoothBandwidth has a very superficial memory of the past:
 * it only remembers the last request. What if the SmoothBandwidth was unused for a long period of
 * time, then a request arrived and was immediately granted? This SmoothBandwidth would immediately
 * forget about that past underutilization. This may result in either underutilization or
 * overflow, depending on the real world consequences of not using the expected rate.
 *
 * Past underutilization could mean that excess resources are available. Then, the SmoothBandwidth
 * should speed up for a while, to take advantage of these resources. This is important when the
 * rate is applied to networking (limiting bandwidth), where past underutilization typically
 * translates to "almost empty buffers", which can be filled immediately.
 *
 * On the other hand, past underutilization could mean that "the server responsible for handling
 * the request has become less ready for future requests", i.e. its caches become stale, and
 * requests become more likely to trigger expensive operations (a more extreme case of this
 * example is when a server has just booted, and it is mostly busy with getting itself up to
 * speed).
 *
 * To deal with such scenarios, we add an extra dimension, that of "past underutilization",
 * modeled by "storedPermits" variable. This variable is zero when there is no underutilization,
 * and it can grow up to maxStoredPermits, for sufficiently large underutilization. So, the
 * requested permits, by an invocation acquire(permits), are served from:
 *
 * - stored permits (if available)
 *
 * - fresh permits (for any remaining permits)
 *
 * How this works is best explained with an example:
 *
 * For a SmoothBandwidth that produces 1 token per second, every second that goes by with the
 * SmoothBandwidth being unused, we increase storedPermits by 1. Say we leave the SmoothBandwidth unused
 * for 10 seconds (i.e., we expected a request at time X, but we are at time X + 10 seconds before
 * a request actually arrives; this is also related to the point made in the last paragraph), thus
 * storedPermits becomes 10.0 (assuming maxStoredPermits >= 10.0). At that point, a request of
 * acquire(3) arrives. We serve this request out of storedPermits, and reduce that to 7.0 (how
 * this is translated to throttling time is discussed later). Immediately after, assume that an
 * acquire(10) request arriving. We serve the request partly from storedPermits, using all the
 * remaining 7.0 permits, and the remaining 3.0, we serve them by fresh permits produced by the
 * rate limiter.
 *
 * We already know how much time it takes to serve 3 fresh permits: if the rate is
 * "1 token per second", then this will take 3 seconds. But what does it mean to serve 7 stored
 * permits? As explained above, there is no unique answer. If we are primarily interested to deal
 * with underutilization, then we want stored permits to be given out /faster/ than fresh ones,
 * because underutilization = free resources for the taking. If we are primarily interested to
 * deal with overflow, then stored permits could be given out /slower/ than fresh ones. Thus, we
 * require a (different in each case) function that translates storedPermits to throttling time.
 *
 * This role is played by storedPermitsToWaitTime(double storedPermits, double permitsToTake). The
 * underlying model is a continuous function mapping storedPermits (from 0.0 to maxStoredPermits)
 * onto the 1/rate (i.e. intervals) that is effective at the given storedPermits. "storedPermits"
 * essentially measure unused time; we spend unused time buying/storing permits. Rate is
 * "permits / time", thus "1 / rate = time / permits". Thus, "1/rate" (time / permits) times
 * "permits" gives time, i.e., integrals on this function (which is what storedPermitsToWaitTime()
 * computes) correspond to minimum intervals between subsequent requests, for the specified number
 * of requested permits.
 *
 * Here is an example of storedPermitsToWaitTime: If storedPermits == 10.0, and we want 3 permits,
 * we take them from storedPermits, reducing them to 7.0, and compute the throttling for these as
 * a call to storedPermitsToWaitTime(storedPermits = 10.0, permitsToTake = 3.0), which will
 * evaluate the integral of the function from 7.0 to 10.0.
 *
 * Using integrals guarantees that the effect of a single acquire(3) is equivalent to {
 * acquire(1); acquire(1); acquire(1); }, or { acquire(2); acquire(1); }, etc, since the integral
 * of the function in [7.0, 10.0] is equivalent to the sum of the integrals of [7.0, 8.0], [8.0,
 * 9.0], [9.0, 10.0] (and so on), no matter what the function is. This guarantees that we handle
 * correctly requests of varying weight (permits), /no matter/ what the actual function is - so we
 * can tweak the latter freely. (The only requirement, obviously, is that we can compute its
 * integrals).
 *
 * Note well that if, for this function, we chose a horizontal line, at height of exactly (1/QPS),
 * then the effect of the function is non-existent: we serve storedPermits at exactly the same
 * cost as fresh ones (1/QPS is the cost for each). We use this trick later.
 *
 * If we pick a function that goes /below/ that horizontal line, it means that we reduce the area
 * of the function, thus time. Thus, the SmoothBandwidth becomes /faster/ after a period of
 * underutilization. If, on the other hand, we pick a function that goes /above/ that horizontal
 * line, then it means that the area (time) is increased, thus storedPermits are more costly than
 * fresh permits, thus the SmoothBandwidth becomes /slower/ after a period of underutilization.
 *
 * Last, but not least: consider a SmoothBandwidth with rate of 1 permit per second, currently
 * completely unused, and an expensive acquire(100) request comes. It would be nonsensical to just
 * wait for 100 seconds, and /then/ start the actual task. Why wait without doing anything? A much
 * better approach is to /allow/ the request right away (as if it was an acquire(1) request
 * instead), and postpone /subsequent/ requests as needed. In this version, we allow starting the
 * task immediately, and postpone by 100 seconds future requests, thus we allow for work to get
 * done in the meantime instead of waiting idly.
 *
 * This has important consequences: it means that the SmoothBandwidth doesn't remember the time of the
 * _last_ request, but it remembers the (expected) time of the _next_ request. This also enables
 * us to tell immediately (see tryAcquire(timeout)) whether a particular timeout is enough to get
 * us to the point of the next scheduling time, since we always maintain that. And what we mean by
 * "an unused SmoothBandwidth" is also defined by that notion: when we observe that the
 * "expected arrival time of the next request" is actually in the past, then the difference (now -
 * past) is the amount of time that the SmoothBandwidth was formally unused, and it is that amount of
 * time which we translate to storedPermits. (We increase storedPermits with the amount of permits
 * that would have been produced in that idle time). So, if rate == 1 permit per second, and
 * arrivals come exactly one second after the previous, then storedPermits is _never_ increased --
 * we would only increase it for arrivals _later_ than the expected one second.
 */
public abstract class SmoothBandwidth implements Bandwidth {

    /**
     * Creates a {@code SmoothBandwidth} with the specified stable throughput, given as "permits per
     * second" (commonly referred to as <i>QPS</i>, queries per second).
     *
     * <p>The returned {@code SmoothBandwidth} ensures that on average no more than {@code
     * permitsPerSecond} are issued during any given second, with sustained requests being smoothly
     * spread over each second. When the incoming request rate exceeds {@code permitsPerSecond} the
     * rate limiter will release one permit every {@code (1.0 / permitsPerSecond)} seconds. When the
     * rate limiter is unused, bursts of up to {@code permitsPerSecond} permits will be allowed, with
     * subsequent requests being smoothly limited at the stable rate of {@code permitsPerSecond}.
     *
     * @param permitsPerSecond the rate of the returned {@code SmoothBandwidth}, measured in how many
     *     permits become available per second
     * @param nowMicros the elapsed time in micros
     * @param maxBurstSeconds the work (permits) of how many seconds can be saved up if this Bandwidth is unused?
     * @throws IllegalArgumentException if {@code permitsPerSecond} is negative or zero
     */
    static SmoothBandwidth bursty(double permitsPerSecond, long nowMicros, double maxBurstSeconds) {
        return new SmoothBurstyBandwidth(permitsPerSecond, nowMicros, maxBurstSeconds);
    }

    /**
     * Creates a {@code SmoothBandwidth} with the specified stable throughput, given as "permits per
     * second" (commonly referred to as <i>QPS</i>, queries per second), and a <i>warmup period</i>,
     * during which the {@code SmoothBandwidth} smoothly ramps up its rate, until it reaches its maximum
     * rate at the end of the period (as long as there are enough requests to saturate it). Similarly,
     * if the {@code SmoothBandwidth} is left <i>unused</i> for a duration of {@code warmupPeriod}, it
     * will gradually return to its "cold" state, i.e. it will go through the same warming up process
     * as when it was first created.
     *
     * <p>The returned {@code SmoothBandwidth} is intended for cases where the resource that actually
     * fulfills the requests (e.g., a remote server) needs "warmup" time, rather than being
     * immediately accessed at the stable (maximum) rate.
     *
     * <p>The returned {@code SmoothBandwidth} starts in a "cold" state (i.e. the warmup period will
     * follow), and if it is left unused for long enough, it will return to that state.
     *
     * @param permitsPerSecond the rate of the returned {@code SmoothBandwidth}, measured in how many
     *     permits become available per second
     * @param nowMicros the elapsed time in micros
     * @param warmupPeriod the duration of the period where the {@code SmoothBandwidth} ramps up its rate,
     *     before reaching its stable (maximum) rate
     * @param timeUnit the time unit of the warmupPeriod argument
     * @param coldFactor
     * @throws IllegalArgumentException if {@code permitsPerSecond} is negative or zero or {@code
     *     warmupPeriod} is negative
     */
    static SmoothBandwidth warmingUp(double permitsPerSecond, long nowMicros,
            long warmupPeriod, TimeUnit timeUnit, double coldFactor) {
        return new SmoothWarmingUpBandwidth(permitsPerSecond, nowMicros, warmupPeriod, timeUnit, coldFactor);
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

    protected SmoothBandwidth() { }

    protected abstract void doSetRate(double permitsPerSecond, double stableIntervalMicros);

    /**
     * Updates the stable rate of this {@code SmoothBandwidth}, that is, the {@code permitsPerSecond}
     * argument provided in the factory method that constructed the {@code SmoothBandwidth}. Currently
     * throttled threads will <b>not</b> be awakened as a result of this invocation, thus they do not
     * observe the new rate; only subsequent requests will.
     *
     * <p>Note though that, since each request repays (by waiting, if necessary) the cost of the
     * <i>previous</i> request, this means that the very next request after an invocation to {@code setPermitsPerSecond}
     * will not be affected by the new rate; it will pay the cost of the previous request,
     * which is in terms of the previous rate.
     *
     * <p>The behavior of the {@code SmoothBandwidth} is not modified in any other way, e.g. if the {@code
     * SmoothBandwidth} was configured with a warmup period of 20 seconds, it still has a warmup period of
     * 20 seconds after this method invocation.
     *
     * @param permitsPerSecond the new stable rate of this {@code SmoothBandwidth}
     * @param nowMicros
     * @throws IllegalArgumentException if {@code permitsPerSecond} is negative or zero
     */
    public void setPermitsPerSecond(double permitsPerSecond, long nowMicros) {
        Checks.requireTrue(permitsPerSecond > 0.0
                && !Double.isNaN(permitsPerSecond), "Must be positive, rate: " + permitsPerSecond);
        resync(nowMicros);
        this.stableIntervalMicros = convert(permitsPerSecond);
        doSetRate(permitsPerSecond, stableIntervalMicros);
    }

    /**
     * Returns the stable rate (as {@code permits per seconds}) with which this {@code Rate} is
     * configured with. The initial value of this is the same as the {@code permitsPerSecond} argument
     * passed in the factory method that produced this {@code Rate}.
     */
    @Override
    public final double getPermitsPerSecond() {
        return convert(stableIntervalMicros);
    }

    private double convert(double rate) {
        return SECONDS.toMicros(1L) / rate;
    }

    @Override
    public final long queryEarliestAvailable(long nowMicros) {
        return nextFreeTicketMicros;
    }

    @Override
    public final long reserveEarliestAvailable(int requiredPermits, long nowMicros) {
        resync(nowMicros);
        final long returnValue = nextFreeTicketMicros;
        final double storedPermitsToSpend = min(requiredPermits, this.storedPermits);
        final double freshPermits = requiredPermits - storedPermitsToSpend;
        final long waitMicros =
                storedPermitsToWaitTime(this.storedPermits, storedPermitsToSpend)
                        + (long) (freshPermits * stableIntervalMicros);

        this.nextFreeTicketMicros = addIgnoringSaturation(nextFreeTicketMicros, waitMicros);
        this.storedPermits -= storedPermitsToSpend;
        return returnValue;
    }

    /**
     * Returns the sum of {@code a} and {@code b} unless it would overflow or underflow in which case
     * {@code Long.MAX_VALUE} or {@code Long.MIN_VALUE} is returned, respectively.
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

    long getNextFreeTicketMicros() {
        return nextFreeTicketMicros;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        SmoothBandwidth that = (SmoothBandwidth) o;
        return Double.compare(that.storedPermits, storedPermits) == 0
                && Double.compare(that.maxPermits, maxPermits) == 0
                && Double.compare(that.stableIntervalMicros, stableIntervalMicros) == 0
                && nextFreeTicketMicros == that.nextFreeTicketMicros;
    }

    @Override
    public int hashCode() {
        return Objects.hash(storedPermits, maxPermits, stableIntervalMicros, nextFreeTicketMicros);
    }
}
