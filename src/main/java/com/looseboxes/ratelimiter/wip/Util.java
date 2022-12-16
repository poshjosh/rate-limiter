package com.looseboxes.ratelimiter.wip;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

class Util {
    /**
     * Returns the number of nanoseconds of the given duration without throwing or overflowing.
     *
     * <p>Instead of throwing {@link ArithmeticException}, this method silently saturates to either
     * {@link Long#MAX_VALUE} or {@link Long#MIN_VALUE}. This behavior can be useful when decomposing
     * a duration in order to call a legacy API which requires a {@code long, TimeUnit} pair.
     */
    static long toNanosSaturated(Duration duration) {
        // Using a try/catch seems lazy, but the catch block will rarely get invoked (except for
        // durations longer than approximately +/- 292 years).
        try {
            return duration.toNanos();
        } catch (ArithmeticException tooBig) {
            return duration.isNegative() ? Long.MIN_VALUE : Long.MAX_VALUE;
        }
    }


    // TODO(user): Support Sleeper somehow (wrapper or interface method)?
    /** Invokes {@code unit.}{@link TimeUnit#sleep(long) sleep(sleepFor)} uninterruptibly. */
    @SuppressWarnings("GoodTime") // should accept a java.time.Duration
    static void sleepUninterruptibly(long sleepFor, TimeUnit unit) {
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
