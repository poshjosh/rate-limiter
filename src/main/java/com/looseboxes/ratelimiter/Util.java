package com.looseboxes.ratelimiter;

import java.time.Duration;

final class Util {
    private Util() { }
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
}
