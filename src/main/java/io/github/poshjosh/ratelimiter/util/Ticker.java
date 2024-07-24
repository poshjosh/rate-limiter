package io.github.poshjosh.ratelimiter.util;

import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static java.util.concurrent.TimeUnit.NANOSECONDS;

/**
 * A Ticker which counts from time created
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
