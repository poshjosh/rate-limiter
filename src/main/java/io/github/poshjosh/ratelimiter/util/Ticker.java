package io.github.poshjosh.ratelimiter.util;

import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static java.util.concurrent.TimeUnit.NANOSECONDS;

/**
 * Counts the progress of time from a fixed arbitrary origin time.
 */
public interface Ticker {

    Ticker SYSTEM_TICKER = System::nanoTime;

    static Ticker systemTicker() {
        return SYSTEM_TICKER;
    }

    default long elapsed(TimeUnit timeUnit) {
        return timeUnit.convert(elapsedNanos(), NANOSECONDS);
    }

    default long elapsedMicros() {
        return elapsed(MICROSECONDS);
    }

    /**
     * @return The elapsed nanoseconds since some fixed but arbitrary origin time
     */
    long elapsedNanos();
}
