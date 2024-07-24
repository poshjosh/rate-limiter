package io.github.poshjosh.ratelimiter.bandwidths;

import io.github.poshjosh.ratelimiter.model.Rate;
import io.github.poshjosh.ratelimiter.model.Rates;
import io.github.poshjosh.ratelimiter.util.Operator;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

import static java.util.concurrent.TimeUnit.SECONDS;

public interface Bandwidths {

    Operator DEFAULT_OPERATOR = Operator.OR;

    Bandwidth UNLIMITED = new Bandwidth() {
        @Override public Bandwidth with(long nowMicros) { return this; }
        @Override public double getPermitsPerSecond() { return Double.MAX_VALUE; }
        @Override public long queryEarliestAvailable(long now) { return now; }
        @Override public long reserveEarliestAvailable(int permits, long now) { return now; }

        @Override public String toString() {
            return "Bandwidth$UNLIMITED";
        }
    };

    static Bandwidth ofSeconds(int permits) {
        return of(Rate.ofSeconds(permits));
    }

    static Bandwidth of(Rate rate) {
        return RateToBandwidthConverter.ofDefaults().convert(rate);
    }

    static Bandwidth of(Rates rates) {
        return RateToBandwidthConverter.ofDefaults().convert(rates);
    }

    static Bandwidth and(Bandwidth... bandwidths) {
        return of(Operator.AND, bandwidths);
    }

    static Bandwidth or(Bandwidth... bandwidths) {
        return of(Operator.OR, bandwidths);
    }

    static Bandwidth of(Bandwidth... bandwidths) {
        return of(Bandwidths.DEFAULT_OPERATOR, bandwidths);
    }

    static Bandwidth of(Operator operator, Bandwidth... bandwidths) {
        return BandwidthArray.of(operator, bandwidths);
    }

    static Bandwidth unmodifiable(Bandwidth bandwidth) {
        return new UnmodifiableBandwidth(bandwidth);
    }

    static Bandwidth allOrNothing(long permitsPerSecond) {
        return allOrNothing(permitsPerSecond, Duration.ofSeconds(1));
    }

    static Bandwidth allOrNothing(long permits, Duration duration) {
        return allOrNothing(permits, duration, 0);
    }

    static Bandwidth allOrNothing(long permits, Duration duration, long nowMicros) {
        return allOrNothing(permits, duration.toNanos(), TimeUnit.NANOSECONDS, nowMicros);
    }

    /**
     * Creates an {@code AllOrNothingBandwidth}, wired to return a value between
     * zero and the stable interval for {@link Bandwidth#queryEarliestAvailable(long)}
     * and {@link Bandwidth#reserveEarliestAvailable(int, long)}
     */
    static Bandwidth allOrNothing(long permits, long duration, TimeUnit timeUnit, long nowMicros) {
        return new AllOrNothingBandwidth(permits, duration, timeUnit, nowMicros);
    }

    static Bandwidth bursty(double permitsPerSecond) {
        return bursty(permitsPerSecond, 0);
    }

    static Bandwidth bursty(double permitsPerSecond, long nowMicros) {
        /*
         * The default bursty Bandwidth configuration can save the unused permits of up to one second. This
         * is to avoid unnecessary stalls in situations like this: A Bandwidth of 1qps, and 4 threads,
         * all calling acquire() at these moments:
         *
         * T0 at 0 seconds
         * T1 at 1.05 seconds
         * T2 at 2 seconds
         * T3 at 3 seconds
         *
         * Due to the slight delay of T1, T2 would have to sleep till 2.05 seconds, and T3 would also
         * have to sleep till 3.05 seconds.
         */
        return bursty(permitsPerSecond, nowMicros, 1.0);
    }

    /**
     * @see SmoothBandwidth#bursty(double, long, double)
     */
    static Bandwidth bursty(double permitsPerSecond, long nowMicros, double maxBurstSeconds) {
        return SmoothBandwidth.bursty(permitsPerSecond, nowMicros, maxBurstSeconds);
    }

    static Bandwidth warmingUp(double permitsPerSecond) {
        return warmingUp(permitsPerSecond, 1);
    }

    static Bandwidth warmingUp(double permitsPerSecond, long warmupPeriodSeconds) {
        return warmingUp(permitsPerSecond, 0, warmupPeriodSeconds);
    }

    static Bandwidth warmingUp(double permitsPerSecond, long nowMicros, long warmupPeriodSeconds) {
        return warmingUp(permitsPerSecond, nowMicros, warmupPeriodSeconds, SECONDS, 3.0);
    }

    /**
     * @see SmoothBandwidth#warmingUp(double, long, long, TimeUnit, double)
     */
    static Bandwidth warmingUp(double permitsPerSecond, long nowMicros, long warmupPeriod,
            TimeUnit timeUnit, double coldFactor) {
        return SmoothBandwidth.warmingUp(permitsPerSecond, nowMicros, warmupPeriod, timeUnit, coldFactor);
    }
}
