package com.looseboxes.ratelimiter.util;

import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MICROSECONDS;

/**
 * A ticker which counts from time created
 */
public abstract class SleepingTicker extends Ticker {

    protected SleepingTicker() { }

    public long elapsedMicros() {
        return elapsed(MICROSECONDS);
    }

    public void sleepMicrosUninterruptibly(long micros) {
        if (micros > 0) {
            sleepUninterruptibly(micros, MICROSECONDS);
        }
    }

    /**
     * Create a ticker which starts ticking from zero. The first call to method {@link Ticker#elapsedNanos()}
     * will return a number as close to zero as possible.
     * @return a ticker which starts ticking from zero.
     */
    public static SleepingTicker zeroOffset() {
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
    private static void sleepUninterruptibly(long sleepFor, TimeUnit unit) {
        boolean interrupted = false;
        try {
            long remainingNanos = unit.toNanos(sleepFor);
            long end = System.nanoTime() + remainingNanos;
            while (true) {
                try {
                    // TimeUnit.sleep() treats negative timeouts just like zero.
                    TimeUnit.NANOSECONDS.sleep(remainingNanos);
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
