package io.github.poshjosh.ratelimiter.util;

import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static java.util.concurrent.TimeUnit.NANOSECONDS;

/**
 * A Ticker which counts from time created
 */
public interface Ticker {

    Ticker SYSTEM_NANO_OFFSET = new Ticker() {
        @Override public long elapsedNanos() { return MILLISECONDS.toNanos(System.currentTimeMillis()); }
        @Override
        public String toString() { return "Ticker$SYSTEM_NANO_OFFSET"; }
    };

    Ticker SYSTEM_EPOCH_MILLIS = new Ticker() {
        @Override public long elapsedNanos() { return System.nanoTime(); }
        @Override
        public String toString() { return "Ticker$SYSTEM_EPOCH_MILLIS"; }
    };

    static Ticker ofDefaults() {
        return SYSTEM_NANO_OFFSET;
    }

    /**
     * Create a Ticker which starts ticking from zero. The first call to method
     * {@link Ticker#elapsedNanos()} will return a number as close to zero as possible.
     * @return a Ticker which starts ticking from zero.
     */
    static Ticker ofZeroOffset() {
        return new Ticker() {
            private final Stopwatch stopwatch = Stopwatch.ofUnstarted();
            @Override
            public long elapsedNanos() {
                if (!stopwatch.isRunning()) {
                    stopwatch.start();
                }
                return stopwatch.elapsedNanos();
            }
            @Override
            public String toString() {
                return "Ticker$ZeroOffset{" + stopwatch + "}";
            }
        };
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
