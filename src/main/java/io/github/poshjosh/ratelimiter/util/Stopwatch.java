package io.github.poshjosh.ratelimiter.util;

import java.time.Duration;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.*;

/**
 * An object that accurately measures <i>elapsed time</i>: the measured duration between two
 * successive readings of "now" in the same process.
 *
 * <p>In contrast, <i>wall time</i> is a reading of "now" as given by a method like
 * {@link System#currentTimeMillis()}, best represented as an {@link java.time.Instant}. Such values
 * <i>can</i> be subtracted to obtain a {@code Duration} (such as by {@code Duration.between}), but
 * doing so does <i>not</i> give a reliable measurement of elapsed time, because wall time readings
 * are inherently approximate, routinely affected by periodic clock corrections. Because this class
 * (by default) uses {@link System#nanoTime}, it is unaffected by these changes.
 *
 * <p>Use this class instead of direct calls to {@link System#nanoTime} for two reasons:
 *
 * <ul>
 *   <li>The raw {@code long} values returned by {@code nanoTime} are meaningless and unsafe to use
 *       in any other way than how {@code Stopwatch} uses them.
 *   <li>An alternative source of nanosecond ticks can be substituted, for example for testing or
 *       performance reasons, without affecting most of your code.
 * </ul>
 *
 * <p>Basic usage:
 *
 * <pre>{@code
 * Stopwatch stopwatch = Stopwatch.createStarted();
 * doSomething();
 * stopwatch.stop(); // optional
 *
 * Duration duration = stopwatch.elapsed();
 *
 * log.info("time: " + stopwatch); // formatted string like "12.3 ms"
 * }</pre>
 *
 * <p>The state-changing methods are not idempotent; it is an error to start or stop a stopwatch
 * that is already in the desired state.
 *
 * <p>When testing code that uses this class, use {@link #ofUnstarted(Ticker)} or {@link
 * #ofStarted(Ticker)} to supply a fake or mock ticker. This allows you to simulate any valid
 * behavior of the stopwatch.
 *
 * <p><b>Note:</b> This class is not thread-safe.
 *
 * <p><b>Warning for Android users:</b> a stopwatch with default behavior may not continue to keep
 * time while the device is asleep. Instead, create one like this:
 *
 * <pre>{@code
 * Stopwatch.createStarted(
 *      new Ticker() {
 *        public long elapsed() {
 *          return android.os.SystemClock.elapsedRealtimeNanos(); // requires API Level 17
 *        }
 *      });
 * }</pre>
 *
 */
@SuppressWarnings("GoodTime")
final class Stopwatch {
    private final Ticker ticker;
    private boolean isRunning;
    private long elapsedNanos;
    private long startTick;

    /**
     * Creates (but does not start) a new stopwatch using {@link System#nanoTime} as its time source.
     */
    public static Stopwatch ofUnstarted() {
        return new Stopwatch();
    }

    /**
     * Creates (but does not start) a new stopwatch, using the specified time source.
     */
    public static Stopwatch ofUnstarted(Ticker ticker) {
        return new Stopwatch(ticker);
    }

    /**
     * Creates (and starts) a new stopwatch using {@link System#nanoTime} as its time source.
     */
    public static Stopwatch ofStarted() {
        return new Stopwatch().start();
    }

    /**
     * Creates (and starts) a new stopwatch, using the specified time source.
     */
    public static Stopwatch ofStarted(Ticker ticker) {
        return new Stopwatch(ticker).start();
    }

    Stopwatch() {
        this(Tickers.SYSTEM_NANO_OFFSET);
    }

    Stopwatch(Ticker ticker) {
        this.ticker = Objects.requireNonNull(ticker, "ticker");
    }

    /**
     * Returns {@code true} if {@link #start()} has been called on this stopwatch, and {@link #stop()}
     * has not been called since the last call to {@code start()}.
     */
    public boolean isRunning() {
        return isRunning;
    }

    /**
     * Starts the stopwatch.
     *
     * @return this {@code Stopwatch} instance
     * @throws IllegalStateException if the stopwatch is already running.
     */
    public Stopwatch start() {
        if (isRunning) {
            throw new IllegalArgumentException("This stopwatch is already running.");
        }
        isRunning = true;
        startTick = ticker.elapsedNanos();
        return this;
    }

    /**
     * Stops the stopwatch. Future reads will return the fixed duration that had elapsed up to this
     * point.
     *
     * @return this {@code Stopwatch} instance
     * @throws IllegalStateException if the stopwatch is already stopped.
     */
    public Stopwatch stop() {
        if (!isRunning) {
            throw new IllegalArgumentException("This stopwatch is already stopped.");
        }
        long tick = ticker.elapsedNanos();
        isRunning = false;
        elapsedNanos += tick - startTick;
        return this;
    }

    /**
     * Sets the elapsed time for this stopwatch to zero, and places it in a stopped state.
     *
     * @return this {@code Stopwatch} instance
     */
    public Stopwatch reset() {
        elapsedNanos = 0;
        isRunning = false;
        return this;
    }

    public long elapsedNanos() {
        return isRunning ? ticker.elapsedNanos() - startTick + elapsedNanos : elapsedNanos;
    }

    /**
     * Returns the current elapsed time shown on this stopwatch, expressed in the desired time unit,
     * with any fraction rounded down.
     *
     * <p><b>Note:</b> the overhead of measurement can be more than a microsecond, so it is generally
     * not useful to specify {@link TimeUnit#NANOSECONDS} precision here.
     *
     * <p>It is generally not a good idea to use an ambiguous, unitless {@code long} to represent
     * elapsed time. Therefore, we recommend using {@link #elapsed()} instead, which returns a
     * strongly-typed {@code Duration} instance.
     */
    public long elapsed(TimeUnit desiredUnit) {
        return desiredUnit.convert(elapsedNanos(), NANOSECONDS);
    }

    /**
     * Returns the current elapsed time shown on this stopwatch as a {@link Duration}. Unlike {@link
     * #elapsed(TimeUnit)}, this method does not lose any precision due to rounding.
     */
    public Duration elapsed() {
        return Duration.ofNanos(elapsedNanos());
    }

    /** Returns a string representation of the current elapsed time. */
    @Override
    public String toString() {
        long nanos = elapsedNanos();

        TimeUnit unit = chooseUnit(nanos);
        double value = (double) nanos / NANOSECONDS.convert(1, unit);

        // Too bad this functionality is not exposed as a regular method call
        return formatCompact4Digits(value) + " " + abbreviate(unit);
    }

    private static TimeUnit chooseUnit(long nanos) {
        if (DAYS.convert(nanos, NANOSECONDS) > 0) {
            return DAYS;
        }
        if (HOURS.convert(nanos, NANOSECONDS) > 0) {
            return HOURS;
        }
        if (MINUTES.convert(nanos, NANOSECONDS) > 0) {
            return MINUTES;
        }
        if (SECONDS.convert(nanos, NANOSECONDS) > 0) {
            return SECONDS;
        }
        if (MILLISECONDS.convert(nanos, NANOSECONDS) > 0) {
            return MILLISECONDS;
        }
        if (MICROSECONDS.convert(nanos, NANOSECONDS) > 0) {
            return MICROSECONDS;
        }
        return NANOSECONDS;
    }

    private static String formatCompact4Digits(double value) {
        return String.format(Locale.ROOT, "%.4g", value);
    }

    private static String abbreviate(TimeUnit unit) {
        switch (unit) {
        case NANOSECONDS:
            return "ns";
        case MICROSECONDS:
            return "\u03bcs"; // μs
        case MILLISECONDS:
            return "ms";
        case SECONDS:
            return "s";
        case MINUTES:
            return "min";
        case HOURS:
            return "h";
        case DAYS:
            return "d";
        default:
            throw new AssertionError();
        }
    }
}
