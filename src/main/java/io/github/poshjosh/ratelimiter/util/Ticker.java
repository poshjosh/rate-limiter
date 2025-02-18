package io.github.poshjosh.ratelimiter.util;

import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static java.util.concurrent.TimeUnit.NANOSECONDS;

/**
 * A Ticker which keeps track of time.
 * <p>A Ticker makes working with Bandwidths easier, as shown in the following example:</p>
 * <pre>
 * <code>
 *     Ticker ticker = Ticker.ofDefaults();
 *     Bandwidth bandwidth = Bandwidths.allOrNothing(1, Duration.ofSeconds(1), ticker.elapsedMicros());
 *     bandwidth.isAvailable(ticker.elapsedMicros());
 * </code>
 * </pre>
 * <p>
 *     <b>Note:</b>Use the same ticker you used when creating the bandwidth to keep track of time
 *     each time you call a bandwidth method which requires the current time. To make life easier,
 *     you could create one ticker and use it throughout your application. There is a default
 *     Ticker which can be accessed via {@link Ticker#ofDefaults()}.
 * </p>
 */
public interface Ticker {
    static Ticker ofDefaults() {
        return Tickers.ofDefaults();
    }
    /**
     * @return The elapsed nanoseconds since some fixed but arbitrary origin time
     */
    long elapsedNanos();

    default long elapsed(TimeUnit timeUnit) {
        return timeUnit.convert(elapsedNanos(), NANOSECONDS);
    }

    default long elapsedMicros() {
        return elapsed(MICROSECONDS);
    }
    
    /** Invokes {@code unit.}{@link TimeUnit#sleep(long) sleep(sleepFor)} without interruption. */
    @SuppressWarnings("GoodTime")
    default void sleepMicrosWithoutInterruption(long sleepFor) {
        if (sleepFor <= 0) {
            return;
        }
        boolean interrupted = false;
        try {
            long remainingNanos = MICROSECONDS.toNanos(sleepFor);
            long end = System.nanoTime() + remainingNanos;
            while (true) {
                try {
                    // TimeUnit.sleep() treats negative timeouts just like zero.
                    NANOSECONDS.sleep(remainingNanos);
                    return;
                } catch (InterruptedException e) {
                    interrupted = true;
                    remainingNanos = end - System.nanoTime();
                }
            }
        } finally {
            if (interrupted) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
