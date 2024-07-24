package io.github.poshjosh.ratelimiter.util;

import static java.util.concurrent.TimeUnit.MILLISECONDS;

public interface Tickers {
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
}
