package com.looseboxes.ratelimiter.util;

import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static java.util.concurrent.TimeUnit.NANOSECONDS;

/**
 * A ticker which counts from time created
 */
public interface SleepingTicker extends Ticker {

    /**
     * Create a ticker which starts ticking from zero. The first call to method {@link Ticker#elapsedNanos()}
     * will return a number as close to zero as possible.
     * @return a ticker which starts ticking from zero.
     */
    static SleepingTicker zeroOffset() {
        return new SleepingTicker() {
            private final Stopwatch stopwatch = Stopwatch.createUnstarted();
            @Override
            public long elapsedNanos() {
                if (!stopwatch.isRunning()) {
                    stopwatch.start();
                }
                return stopwatch.elapsedNanos();
            }
            @Override
            public String toString() {
                return "SleepingTicker@" + Integer.toHexString(hashCode()) + "{" + stopwatch + "}";
            }
        };
    }

    /** Invokes {@code unit.}{@link TimeUnit#sleep(long) sleep(sleepFor)} uninterruptibly. */
    @SuppressWarnings("GoodTime")
    default void sleepMicrosUninterruptibly(long sleepFor) {
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
