package com.looseboxes.ratelimiter.util;

import java.util.concurrent.TimeUnit;

/**
 * Counts the progress of time from a fixed arbitrary origin time.
 */
public abstract class Ticker {

    private static final Ticker SYSTEM_TICKER = new Ticker() {
        public long elapsedNanos() {
            return System.nanoTime();
        }
    };

    protected Ticker() {}

    public long elapsed(TimeUnit timeUnit) {
        return timeUnit.convert(elapsedNanos(), TimeUnit.NANOSECONDS);
    }

    /**
     * @return The elapsed nanoseconds since some fixed but arbitrary origin time
     */
    public abstract long elapsedNanos();

    public static Ticker systemTicker() {
        return SYSTEM_TICKER;
    }
}
